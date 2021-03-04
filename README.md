# vk-generator
A generator for low-level Common Lisp/CFFI bindings for the Vulkan API.

## Important Notes

This project is still under development.
Most bindings have not been tested at all.
Support for Windows and MacOS is completely untested.

I'm currently testing with SDK version 1.2.153 only.
Support for other versions is not guaranteed at the moment, but the goal is to support at least every new version of the SDK.

## Memory Management in VK

TODO: describe vk-alloc stuff, special care when multithreading (e.g. bordeaux-threads:*default-special-bindings*)

## Misc

`VK` shadows: `format`, `set`, `stream`, `type` `values`

All accessors to slots named `wait-semaphores` (i.e. `pWaitSemaphores` in the C API) are called `p-wait-semaphores` because there is also a function named `vk:wait-semaphores`.

## Known Issues

### Pointers to Arrays of Pointers

Bindings for pointers to arrays of pointers are currently not created correctly.
Up to SDK version 1.2.153 there is one case (`VkAccelerationStructureBuildGeometryInfoKHR.ppGeometries`) where this would be required.

Because of this issue Vulkan ray tracing is currently not supported.

### size-t

If `generate` is not called from within the package `vk-generator` the type `size-t` will currently be incorrectly written as `vk-generator:size-t`.

## Acknowledgements

The project has been forked from [cl-vulkan](https://github.com/3b/cl-vulkan).

The code responsible for parsing and representing the Vulkan API registry has been ported from [Vulkan-Hpp](https://github.com/KhronosGroup/Vulkan-Hpp).
