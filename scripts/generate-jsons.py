#!/usr/bin/env python3
#
# Copyright 2026 The Khronos Group Inc.
#
# SPDX-License-Identifier: Apache-2.0 OR MIT
#
# Generate the vulkan_object.py JSON dump consumed by the `vulkan-json-types`
# package: a base `vk.json` plus one `vk-<field>.json` shard per large field.
#
# This drives the registry/generator machinery that ships in the Vulkan-Docs
# submodule (generate-new/Vulkan-Docs/scripts) to build a `VulkanObject`, then
# serialises it to JSON. Every dataclass node is tagged with a `__type__`
# discriminator (its class name). The 15 top-level "entity" types are emitted
# in full where they are *defined* (as the value of a top-level collection) and
# collapsed to a `{"__ref__": <Class>, "name": <name>}` stub wherever they are
# *referenced* from inside another entity -- the JSON analogue of the
# `_CompactPrinter` in `print_vulkan_object.py`.
#
# The big collections are written to sibling shard files and emptied in
# `vk.json`, keeping that file small while preserving the full field layout.

import argparse
import json
import os
import sys
import tempfile
from dataclasses import fields, is_dataclass
from enum import Enum as PyEnum
from xml.etree import ElementTree

HERE = os.path.dirname(os.path.abspath(__file__))
REPO_ROOT = os.path.dirname(HERE)
VK_DOCS_SCRIPTS = os.path.join(REPO_ROOT, 'generate-new', 'Vulkan-Docs', 'scripts')

# The registry machinery imports its siblings by bare module name.
sys.path.insert(0, VK_DOCS_SCRIPTS)

from reg import Registry                       # noqa: E402
from base_generator import (                   # noqa: E402
    BaseGenerator,
    BaseGeneratorOptions,
    SetOutputDirectory,
    SetOutputFileName,
    SetTargetApiName,
    SetMergedApiNames,
)
import vulkan_object as vko                     # noqa: E402

# Top-level named entity types. Encountered as the value of a top-level
# collection they are serialised in full; encountered nested inside another
# entity they are serialised as a compact {"__ref__": ..., "name": ...} stub.
# This mirrors `_ENTITY_TYPES` in print_vulkan_object.py.
ENTITY_TYPES = (
    vko.Version, vko.Extension, vko.Handle, vko.Command, vko.Struct,
    vko.Enum, vko.Bitmask, vko.Flags, vko.Constant, vko.Format, vko.FuncPointer,
    vko.SyncStage, vko.SyncAccess, vko.SyncPipeline, vko.Spirv,
)

# VulkanObject fields that are split out into their own `vk-<field>.json` shard
# (and emptied in vk.json). The remaining collections are small enough to keep
# inline in vk.json.
SHARD_FIELDS = (
    'extensions', 'versions', 'handles', 'commands', 'structs', 'enums',
    'bitmasks', 'flags', 'formats', 'syncStage', 'syncAccess', 'spirv',
)

# These three are combined into a single `vk-aliases.json` shard.
ALIAS_FIELDS = (
    'aliasTypeRequirements', 'aliasFieldRequirements', 'aliasFlagRequirements',
)


def encode(obj, depth):
    """Recursively turn a VulkanObject graph into json-serialisable data.

    `depth` counts how many *entity* objects we are nested inside. An entity at
    depth 0 (i.e. as a top-level collection value) is expanded in full; the same
    entity at depth > 0 (referenced from within another entity) becomes a stub.
    Non-entity dataclasses (Member, Param, Flag, ...) and the root VulkanObject
    are always expanded and do not change the depth.
    """
    if is_dataclass(obj) and not isinstance(obj, type):
        cls = type(obj).__name__
        is_entity = isinstance(obj, ENTITY_TYPES)
        if is_entity and depth > 0:
            return {'__ref__': cls, 'name': getattr(obj, 'name', None)}
        inner = depth + 1 if is_entity else depth
        out = {'__type__': cls}
        for f in fields(obj):
            out[f.name] = encode(getattr(obj, f.name), inner)
        return out
    if isinstance(obj, PyEnum):
        return obj.name
    if isinstance(obj, dict):
        return {k: encode(v, depth) for k, v in obj.items()}
    if isinstance(obj, (list, tuple)):
        return [encode(v, depth) for v in obj]
    if isinstance(obj, (set, frozenset)):
        return [encode(v, depth) for v in sorted(obj)]
    return obj


def sort_collections(root):
    """Sort the top-level VulkanObject collections for stable output.

    Sorting is applied only at the top level so that order-significant nested
    lists (e.g. a struct's members) are left untouched:

      * dict collections     -> sorted by key
      * lists of named items -> sorted by name (e.g. syncPipeline, spirv)
      * lists of strings      -> sorted (e.g. vendorTags)
      * lists without a name  -> left in registry order (syncStage, syncAccess)
    """
    for key, value in root.items():
        if isinstance(value, dict):
            root[key] = dict(sorted(value.items()))
        elif isinstance(value, list) and value:
            if all(isinstance(e, dict) and 'name' in e for e in value):
                root[key] = sorted(value, key=lambda e: e['name'])
            elif all(isinstance(e, str) for e in value):
                root[key] = sorted(value)
    return root


def write_json(path, data):
    with open(path, 'w') as f:
        json.dump(data, f, indent=2)
        f.write('\n')


def dump_vulkan_object(vk, out_dir):
    os.makedirs(out_dir, exist_ok=True)
    root = sort_collections(encode(vk, 0))

    written = []
    for field_name in SHARD_FIELDS:
        shard = root[field_name]
        write_json(os.path.join(out_dir, f'vk-{field_name}.json'), shard)
        root[field_name] = [] if isinstance(shard, list) else {}
        written.append(f'vk-{field_name}.json')

    aliases = {name: root[name] for name in ALIAS_FIELDS}
    write_json(os.path.join(out_dir, 'vk-aliases.json'), aliases)
    for name in ALIAS_FIELDS:
        root[name] = {}
    written.append('vk-aliases.json')

    write_json(os.path.join(out_dir, 'vk.json'), root)
    written.append('vk.json')
    return written


class JsonGenerator(BaseGenerator):
    def __init__(self, out_dir):
        BaseGenerator.__init__(self)
        self.out_dir = out_dir

    def generate(self):
        written = dump_vulkan_object(self.vk, self.out_dir)
        for name in written:
            print(f'  wrote {os.path.join(self.out_dir, name)}', file=sys.stderr)


def main():
    default_xml = os.path.join(VK_DOCS_SCRIPTS, '..', 'xml', 'vk.xml')
    default_out = os.path.join(REPO_ROOT, 'jsons')

    parser = argparse.ArgumentParser(
        description='Generate the Vulkan JSON dump (vk.json + vk-*.json shards).')
    parser.add_argument('-o', '--out-dir', default=default_out, dest='out_dir',
                        help='output directory (default: <repo>/jsons)')
    parser.add_argument('--xml', default=os.path.normpath(default_xml),
                        help='path to the XML registry (default: Vulkan-Docs xml/vk.xml)')
    parser.add_argument('--api', default='vulkan',
                        choices=['vulkan', 'vulkansc', 'vulkanbase'],
                        help='target API (default: vulkan)')
    args = parser.parse_args()

    out_dir = os.path.abspath(args.out_dir)
    print(f'Generating Vulkan JSON dump from {args.xml}', file=sys.stderr)

    with tempfile.TemporaryDirectory() as tmp_dir:
        # The generator machinery insists on an output directory/file even
        # though we only want the in-memory VulkanObject it builds for us.
        SetOutputDirectory(tmp_dir)
        SetOutputFileName('workaround.txt')
        SetTargetApiName(args.api)
        SetMergedApiNames(None)

        generator = JsonGenerator(out_dir)
        reg = Registry(generator, BaseGeneratorOptions())
        reg.loadElementTree(ElementTree.parse(args.xml))
        reg.apiGen()


if __name__ == '__main__':
    main()
