# Generate

This program generates the haskell source from the Vulkan xml specification. 

## Developing

To regenerate the  bindings:

```
./generate.sh \
  /home/j/projects/vulkan/src/ \
  /home/j/projects/vulkan/vulkan.cabal \
  /home/j/src/Vulkan-Docs
```

To see Pandoc's representation of a man page

```
runhaskell \
  src/Documentation/RunAsciiDoctor.hs \
  ~/src/Vulkan-Docs/ \
  ~/src/Vulkan-Docs/man/vkCmdSetDepthBias.txt \
  | pandoc --from docbook --to native`
```
