# Generate

This program generates the haskell source from the Vulkan xml specification. 

## Developing

To see Pandoc's representation of a man page

```
runhaskell \
  src/Documentation/RunAsciiDoctor.hs \
  ~/src/Vulkan-Docs/ \
  ~/src/Vulkan-Docs/man/vkCmdSetDepthBias.txt \
  | pandoc --from docbook --to native`
```
