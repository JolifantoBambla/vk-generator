# vk-generator
A generator for low-level Common Lisp/CFFI bindings for the Vulkan API.

## Known Issues

### size-t

If `generate` is not called from within the package `vk-generator` the type `size-t` will currently be incorrectly written as `vk-generator:size-t`.

## Acknowledgements

The project has been forked from [cl-vulkan](https://github.com/3b/cl-vulkan).

The code responsible for parsing and representing the Vulkan API registry has been ported from [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).
