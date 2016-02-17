;;; this file is automatically generated, do not edit
#||
Copyright (c) 2015-2016 The Khronos Group Inc.

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and/or associated documentation files (the
"Materials"), to deal in the Materials without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Materials, and to
permit persons to whom the Materials are furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Materials.

THE MATERIALS ARE PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
MATERIALS OR THE USE OR OTHER DEALINGS IN THE MATERIALS.

------------------------------------------------------------------------
||#

(in-package #:cl-vulkan-bindings)

(defbitfield (access-flags flags)
  (:indirect-command-read #x1) ;; Controls coherency of indirect command reads
  (:index-read #x2) ;; Controls coherency of index reads
  (:vertex-attribute-read #x4) ;; Controls coherency of vertex attribute reads
  (:uniform-read #x8) ;; Controls coherency of uniform buffer reads
  (:input-attachment-read #x10) ;; Controls coherency of input attachment reads
  (:shader-read #x20) ;; Controls coherency of shader reads
  (:shader-write #x40) ;; Controls coherency of shader writes
  (:color-attachment-read #x80) ;; Controls coherency of color attachment reads
  (:color-attachment-write #x100) ;; Controls coherency of color attachment writes
  (:depth-stencil-attachment-read #x200) ;; Controls coherency of depth/stencil attachment reads
  (:depth-stencil-attachment-write #x400) ;; Controls coherency of depth/stencil attachment writes
  (:transfer-read #x800) ;; Controls coherency of transfer reads
  (:transfer-write #x1000) ;; Controls coherency of transfer writes
  (:host-read #x2000) ;; Controls coherency of host reads
  (:host-write #x4000) ;; Controls coherency of host writes
  (:memory-read #x8000) ;; Controls coherency of memory reads
  (:memory-write #x10000)) ;; Controls coherency of memory writes

(defbitfield (android-surface-create-flags-khr flags))

(defbitfield (attachment-description-flags flags)
  (:may-alias #x1)) ;; The attachment may alias physical memory of another attachment in the same render pass

(defbitfield (buffer-create-flags flags)
  (:sparse-binding #x1) ;; Buffer should support sparse backing
  (:sparse-residency #x2) ;; Buffer should support sparse backing with partial residency
  (:sparse-aliased #x4)) ;; Buffer should support constent data access to physical memory blocks mapped into multiple locations of sparse buffers

(defbitfield (buffer-usage-flags flags)
  (:transfer-src #x1) ;; Can be used as a source of transfer operations
  (:transfer-dst #x2) ;; Can be used as a destination of transfer operations
  (:uniform-texel-buffer #x4) ;; Can be used as TBO
  (:storage-texel-buffer #x8) ;; Can be used as IBO
  (:uniform-buffer #x10) ;; Can be used as UBO
  (:storage-buffer #x20) ;; Can be used as SSBO
  (:index-buffer #x40) ;; Can be used as source of fixed-function index fetch (index buffer)
  (:vertex-buffer #x80) ;; Can be used as source of fixed-function vertex fetch (VBO)
  (:indirect-buffer #x100)) ;; Can be the source of indirect parameters (e.g. indirect buffer, parameter buffer)

(defbitfield (buffer-view-create-flags flags))

(defbitfield (color-component-flags flags)
  (:r #x1)
  (:g #x2)
  (:b #x4)
  (:a #x8))

(defbitfield (command-buffer-reset-flags flags)
  (:release-resources #x1)) ;; Release resources owned by the buffer

(defbitfield (command-buffer-usage-flags flags)
  (:one-time-submit #x1)
  (:render-pass-continue #x2)
  (:simultaneous-use #x4)) ;; Command buffer may be submitted/executed more than once simultaneously

(defbitfield (command-pool-create-flags flags)
  (:transient #x1) ;; Command buffers have a short lifetime
  (:reset-command-buffer #x2)) ;; Command buffers may release their memory individually

(defbitfield (command-pool-reset-flags flags)
  (:release-resources #x1)) ;; Release resources owned by the pool

(defbitfield (composite-alpha-flags-khr flags)
  (:opaque #x1)
  (:pre-multiplied #x2)
  (:post-multiplied #x4)
  (:inherit #x8))

(defbitfield (cull-mode-flags flags)
  (:none #x0)
  (:front #x1)
  (:back #x2)
  (:front-and-back #x3))

(defbitfield (debug-report-flags-ext flags)
  (:information #x1)
  (:warning #x2)
  (:performance-warning #x4)
  (:error #x8)
  (:debug #x10))

(defbitfield (dependency-flags flags)
  (:by-region #x1)) ;; Dependency is per pixel region

(defbitfield (descriptor-pool-create-flags flags)
  (:free-descriptor-set #x1)) ;; Descriptor sets may be freed individually

(defbitfield (descriptor-pool-reset-flags flags))

(defbitfield (descriptor-set-layout-create-flags flags))

(defbitfield (device-create-flags flags))

(defbitfield (device-queue-create-flags flags))

(defbitfield (display-mode-create-flags-khr flags))

(defbitfield (display-plane-alpha-flags-khr flags)
  (:opaque #x1)
  (:global #x2)
  (:per-pixel #x4)
  (:per-pixel-premultiplied #x8))

(defbitfield (display-surface-create-flags-khr flags))

(defbitfield (event-create-flags flags))

(defbitfield (fence-create-flags flags)
  (:signaled #x1))

(defbitfield (format-feature-flags flags)
  (:sampled-image #x1) ;; Format can be used for sampled images (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
  (:storage-image #x2) ;; Format can be used for storage images (STORAGE_IMAGE descriptor type)
  (:storage-image-atomic #x4) ;; Format supports atomic operations in case it's used for storage images
  (:uniform-texel-buffer #x8) ;; Format can be used for uniform texel buffers (TBOs)
  (:storage-texel-buffer #x10) ;; Format can be used for storage texel buffers (IBOs)
  (:storage-texel-buffer-atomic #x20) ;; Format supports atomic operations in case it's used for storage texel buffers
  (:vertex-buffer #x40) ;; Format can be used for vertex buffers (VBOs)
  (:color-attachment #x80) ;; Format can be used for color attachment images
  (:color-attachment-blend #x100) ;; Format supports blending in case it's used for color attachment images
  (:depth-stencil-attachment #x200) ;; Format can be used for depth/stencil attachment images
  (:blit-src #x400) ;; Format can be used as the source image of blits with vkCmdBlitImage
  (:blit-dst #x800) ;; Format can be used as the destination image of blits with vkCmdBlitImage
  (:sampled-image-filter-linear #x1000)) ;; Format can be filtered with VK_FILTER_LINEAR when being sampled

(defbitfield (framebuffer-create-flags flags))

(defbitfield (image-aspect-flags flags)
  (:color #x1)
  (:depth #x2)
  (:stencil #x4)
  (:metadata #x8))

(defbitfield (image-create-flags flags)
  (:sparse-binding #x1) ;; Image should support sparse backing
  (:sparse-residency #x2) ;; Image should support sparse backing with partial residency
  (:sparse-aliased #x4) ;; Image should support constent data access to physical memory blocks mapped into multiple locations of sparse images
  (:mutable-format #x8) ;; Allows image views to have different format than the base image
  (:cube-compatible #x10)) ;; Allows creating image views with cube type from the created image

(defbitfield (image-usage-flags flags)
  (:transfer-src #x1) ;; Can be used as a source of transfer operations
  (:transfer-dst #x2) ;; Can be used as a destination of transfer operations
  (:sampled #x4) ;; Can be sampled from (SAMPLED_IMAGE and COMBINED_IMAGE_SAMPLER descriptor types)
  (:storage #x8) ;; Can be used as storage image (STORAGE_IMAGE descriptor type)
  (:color-attachment #x10) ;; Can be used as framebuffer color attachment
  (:depth-stencil-attachment #x20) ;; Can be used as framebuffer depth/stencil attachment
  (:transient-attachment #x40) ;; Image data not needed outside of rendering
  (:input-attachment #x80)) ;; Can be used as framebuffer input attachment

(defbitfield (image-view-create-flags flags))

(defbitfield (instance-create-flags flags))

(defbitfield (memory-heap-flags flags)
  (:device-local #x1)) ;; If set, heap represents device memory

(defbitfield (memory-map-flags flags))

(defbitfield (memory-property-flags flags)
  (:device-local #x1) ;; If otherwise stated, then allocate memory on device
  (:host-visible #x2) ;; Memory is mappable by host
  (:host-coherent #x4) ;; Memory will have i/o coherency. If not set, application may need to use vkFlushMappedMemoryRanges and vkInvalidateMappedMemoryRanges to flush/invalidate host cache
  (:host-cached #x8) ;; Memory will be cached by the host
  (:lazily-allocated #x10)) ;; Memory may be allocated by the driver when it is required

(defbitfield (mir-surface-create-flags-khr flags))

(defbitfield (pipeline-cache-create-flags flags))

(defbitfield (pipeline-color-blend-state-create-flags flags))

(defbitfield (pipeline-create-flags flags)
  (:disable-optimization #x1)
  (:allow-derivatives #x2)
  (:derivative #x4))

(defbitfield (pipeline-depth-stencil-state-create-flags flags))

(defbitfield (pipeline-dynamic-state-create-flags flags))

(defbitfield (pipeline-input-assembly-state-create-flags flags))

(defbitfield (pipeline-layout-create-flags flags))

(defbitfield (pipeline-multisample-state-create-flags flags))

(defbitfield (pipeline-rasterization-state-create-flags flags))

(defbitfield (pipeline-shader-stage-create-flags flags))

(defbitfield (pipeline-stage-flags flags)
  (:top-of-pipe #x1) ;; Before subsequent commands are processed
  (:draw-indirect #x2) ;; Draw/DispatchIndirect command fetch
  (:vertex-input #x4) ;; Vertex/index fetch
  (:vertex-shader #x8) ;; Vertex shading
  (:tessellation-control-shader #x10) ;; Tessellation control shading
  (:tessellation-evaluation-shader #x20) ;; Tessellation evaluation shading
  (:geometry-shader #x40) ;; Geometry shading
  (:fragment-shader #x80) ;; Fragment shading
  (:early-fragment-tests #x100) ;; Early fragment (depth and stencil) tests
  (:late-fragment-tests #x200) ;; Late fragment (depth and stencil) tests
  (:color-attachment-output #x400) ;; Color attachment writes
  (:compute-shader #x800) ;; Compute shading
  (:transfer #x1000) ;; Transfer/copy operations
  (:bottom-of-pipe #x2000) ;; After previous commands have completed
  (:host #x4000) ;; Indicates host (CPU) is a source/sink of the dependency
  (:all-graphics #x8000) ;; All stages of the graphics pipeline
  (:all-commands #x10000)) ;; All stages supported on the queue

(defbitfield (pipeline-tessellation-state-create-flags flags))

(defbitfield (pipeline-vertex-input-state-create-flags flags))

(defbitfield (pipeline-viewport-state-create-flags flags))

(defbitfield (query-control-flags flags)
  (:precise #x1)) ;; Require precise results to be collected by the query

(defbitfield (query-pipeline-statistic-flags flags)
  (:input-assembly-vertices #x1) ;; Optional
  (:input-assembly-primitives #x2) ;; Optional
  (:vertex-shader-invocations #x4) ;; Optional
  (:geometry-shader-invocations #x8) ;; Optional
  (:geometry-shader-primitives #x10) ;; Optional
  (:clipping-invocations #x20) ;; Optional
  (:clipping-primitives #x40) ;; Optional
  (:fragment-shader-invocations #x80) ;; Optional
  (:tessellation-control-shader-patches #x100) ;; Optional
  (:tessellation-evaluation-shader-invocations #x200) ;; Optional
  (:compute-shader-invocations #x400)) ;; Optional

(defbitfield (query-pool-create-flags flags))

(defbitfield (query-result-flags flags)
  (:64 #x1) ;; Results of the queries are written to the destination buffer as 64-bit values
  (:wait #x2) ;; Results of the queries are waited on before proceeding with the result copy
  (:with-availability #x4) ;; Besides the results of the query, the availability of the results is also written
  (:partial #x8)) ;; Copy the partial results of the query even if the final results aren't available

(defbitfield (queue-flags flags)
  (:graphics #x1) ;; Queue supports graphics operations
  (:compute #x2) ;; Queue supports compute operations
  (:transfer #x4) ;; Queue supports transfer operations
  (:sparse-binding #x8)) ;; Queue supports sparse resource memory management operations

(defbitfield (render-pass-create-flags flags))

(defbitfield (sample-count-flags flags)
  (:1 #x1) ;; Sample count 1 supported
  (:2 #x2) ;; Sample count 2 supported
  (:4 #x4) ;; Sample count 4 supported
  (:8 #x8) ;; Sample count 8 supported
  (:16 #x10) ;; Sample count 16 supported
  (:32 #x20) ;; Sample count 32 supported
  (:64 #x40)) ;; Sample count 64 supported

(defbitfield (sampler-create-flags flags))

(defbitfield (semaphore-create-flags flags))

(defbitfield (shader-module-create-flags flags))

(defbitfield (shader-stage-flags flags)
  (:vertex #x1)
  (:tessellation-control #x2)
  (:tessellation-evaluation #x4)
  (:geometry #x8)
  (:fragment #x10)
  (:compute #x20)
  (:all-graphics #x1F)
  (:all #x7FFFFFFF))

(defbitfield (sparse-image-format-flags flags)
  (:single-miptail #x1) ;; Image uses a single miptail region for all array layers
  (:aligned-mip-size #x2) ;; Image requires mip levels to be an exact multiple of the sparse image block size for non-miptail levels.
  (:nonstandard-block-size #x4)) ;; Image uses a non-standard sparse block size

(defbitfield (sparse-memory-bind-flags flags)
  (:metadata #x1)) ;; Operation binds resource metadata to memory

(defbitfield (stencil-face-flags flags)
  (:front #x1) ;; Front face
  (:back #x2) ;; Back face
  (:vk-stencil-front-and-back #x3)) ;; Front and back faces

(defbitfield (subpass-description-flags flags))

(defbitfield (surface-transform-flags-khr flags)
  (:identity #x1)
  (:rotate-90 #x2)
  (:rotate-180 #x4)
  (:rotate-270 #x8)
  (:horizontal-mirror #x10)
  (:horizontal-mirror-rotate-90 #x20)
  (:horizontal-mirror-rotate-180 #x40)
  (:horizontal-mirror-rotate-270 #x80)
  (:inherit #x100))

(defbitfield (swapchain-create-flags-khr flags))

(defbitfield (wayland-surface-create-flags-khr flags))

(defbitfield (win-3-2-surface-create-flags-khr flags))

(defbitfield (xcb-surface-create-flags-khr flags))

(defbitfield (xlib-surface-create-flags-khr flags))

(defcenum (attachment-load-op)
  (:load #x0)
  (:clear #x1)
  (:dont-care #x2))

(defcenum (attachment-store-op)
  (:store #x0)
  (:dont-care #x1))

(defcenum (blend-factor)
  (:zero #x0)
  (:one #x1)
  (:src-color #x2)
  (:one-minus-src-color #x3)
  (:dst-color #x4)
  (:one-minus-dst-color #x5)
  (:src-alpha #x6)
  (:one-minus-src-alpha #x7)
  (:dst-alpha #x8)
  (:one-minus-dst-alpha #x9)
  (:constant-color #xA)
  (:one-minus-constant-color #xB)
  (:constant-alpha #xC)
  (:one-minus-constant-alpha #xD)
  (:src-alpha-saturate #xE)
  (:src1-color #xF)
  (:one-minus-src1-color #x10)
  (:src1-alpha #x11)
  (:one-minus-src1-alpha #x12))

(defcenum (blend-op)
  (:add #x0)
  (:subtract #x1)
  (:reverse-subtract #x2)
  (:min #x3)
  (:max #x4))

(defcenum (border-color)
  (:float-transparent-black #x0)
  (:int-transparent-black #x1)
  (:float-opaque-black #x2)
  (:int-opaque-black #x3)
  (:float-opaque-white #x4)
  (:int-opaque-white #x5))

(defcenum (color-space-khr)
  (:srgb-nonlinear-khr #x0))

(defcenum (command-buffer-level)
  (:primary #x0)
  (:secondary #x1))

(defcenum (compare-op)
  (:never #x0)
  (:less #x1)
  (:equal #x2)
  (:less-or-equal #x3)
  (:greater #x4)
  (:not-equal #x5)
  (:greater-or-equal #x6)
  (:always #x7))

(defcenum (component-swizzle)
  (:identity #x0)
  (:zero #x1)
  (:one #x2)
  (:r #x3)
  (:g #x4)
  (:b #x5)
  (:a #x6))

(defcenum (debug-report-error-ext)
  (:debug-report-error-none-ext #x0)
  (:debug-report-error-callback-ref-ext #x1))

(defcenum (debug-report-object-type-ext)
  (:debug-report-object-type-unknown-ext #x0)
  (:debug-report-object-type-instance-ext #x1)
  (:debug-report-object-type-physical-device-ext #x2)
  (:debug-report-object-type-device-ext #x3)
  (:debug-report-object-type-queue-ext #x4)
  (:debug-report-object-type-semaphore-ext #x5)
  (:debug-report-object-type-command-buffer-ext #x6)
  (:debug-report-object-type-fence-ext #x7)
  (:debug-report-object-type-device-memory-ext #x8)
  (:debug-report-object-type-buffer-ext #x9)
  (:debug-report-object-type-image-ext #xA)
  (:debug-report-object-type-event-ext #xB)
  (:debug-report-object-type-query-pool-ext #xC)
  (:debug-report-object-type-buffer-view-ext #xD)
  (:debug-report-object-type-image-view-ext #xE)
  (:debug-report-object-type-shader-module-ext #xF)
  (:debug-report-object-type-pipeline-cache-ext #x10)
  (:debug-report-object-type-pipeline-layout-ext #x11)
  (:debug-report-object-type-render-pass-ext #x12)
  (:debug-report-object-type-pipeline-ext #x13)
  (:debug-report-object-type-descriptor-set-layout-ext #x14)
  (:debug-report-object-type-sampler-ext #x15)
  (:debug-report-object-type-descriptor-pool-ext #x16)
  (:debug-report-object-type-descriptor-set-ext #x17)
  (:debug-report-object-type-framebuffer-ext #x18)
  (:debug-report-object-type-command-pool-ext #x19)
  (:debug-report-object-type-surface-khr-ext #x1A)
  (:debug-report-object-type-swapchain-khr-ext #x1B)
  (:debug-report-object-type-debug-report-ext #x1C))

(defcenum (descriptor-type)
  (:sampler #x0)
  (:combined-image-sampler #x1)
  (:sampled-image #x2)
  (:storage-image #x3)
  (:uniform-texel-buffer #x4)
  (:storage-texel-buffer #x5)
  (:uniform-buffer #x6)
  (:storage-buffer #x7)
  (:uniform-buffer-dynamic #x8)
  (:storage-buffer-dynamic #x9)
  (:input-attachment #xA))

(defcenum (dynamic-state)
  (:viewport #x0)
  (:scissor #x1)
  (:line-width #x2)
  (:depth-bias #x3)
  (:blend-constants #x4)
  (:depth-bounds #x5)
  (:stencil-compare-mask #x6)
  (:stencil-write-mask #x7)
  (:stencil-reference #x8))

(defcenum (filter)
  (:nearest #x0)
  (:linear #x1))

(defcenum (format)
  (:undefined #x0)
  (:r4g4-unorm-pack8 #x1)
  (:r4g4b4a4-unorm-pack16 #x2)
  (:b4g4r4a4-unorm-pack16 #x3)
  (:r5g6b5-unorm-pack16 #x4)
  (:b5g6r5-unorm-pack16 #x5)
  (:r5g5b5a1-unorm-pack16 #x6)
  (:b5g5r5a1-unorm-pack16 #x7)
  (:a1r5g5b5-unorm-pack16 #x8)
  (:r8-unorm #x9)
  (:r8-snorm #xA)
  (:r8-uscaled #xB)
  (:r8-sscaled #xC)
  (:r8-uint #xD)
  (:r8-sint #xE)
  (:r8-srgb #xF)
  (:r8g8-unorm #x10)
  (:r8g8-snorm #x11)
  (:r8g8-uscaled #x12)
  (:r8g8-sscaled #x13)
  (:r8g8-uint #x14)
  (:r8g8-sint #x15)
  (:r8g8-srgb #x16)
  (:r8g8b8-unorm #x17)
  (:r8g8b8-snorm #x18)
  (:r8g8b8-uscaled #x19)
  (:r8g8b8-sscaled #x1A)
  (:r8g8b8-uint #x1B)
  (:r8g8b8-sint #x1C)
  (:r8g8b8-srgb #x1D)
  (:b8g8r8-unorm #x1E)
  (:b8g8r8-snorm #x1F)
  (:b8g8r8-uscaled #x20)
  (:b8g8r8-sscaled #x21)
  (:b8g8r8-uint #x22)
  (:b8g8r8-sint #x23)
  (:b8g8r8-srgb #x24)
  (:r8g8b8a8-unorm #x25)
  (:r8g8b8a8-snorm #x26)
  (:r8g8b8a8-uscaled #x27)
  (:r8g8b8a8-sscaled #x28)
  (:r8g8b8a8-uint #x29)
  (:r8g8b8a8-sint #x2A)
  (:r8g8b8a8-srgb #x2B)
  (:b8g8r8a8-unorm #x2C)
  (:b8g8r8a8-snorm #x2D)
  (:b8g8r8a8-uscaled #x2E)
  (:b8g8r8a8-sscaled #x2F)
  (:b8g8r8a8-uint #x30)
  (:b8g8r8a8-sint #x31)
  (:b8g8r8a8-srgb #x32)
  (:a8b8g8r8-unorm-pack32 #x33)
  (:a8b8g8r8-snorm-pack32 #x34)
  (:a8b8g8r8-uscaled-pack32 #x35)
  (:a8b8g8r8-sscaled-pack32 #x36)
  (:a8b8g8r8-uint-pack32 #x37)
  (:a8b8g8r8-sint-pack32 #x38)
  (:a8b8g8r8-srgb-pack32 #x39)
  (:a2r10g10b10-unorm-pack32 #x3A)
  (:a2r10g10b10-snorm-pack32 #x3B)
  (:a2r10g10b10-uscaled-pack32 #x3C)
  (:a2r10g10b10-sscaled-pack32 #x3D)
  (:a2r10g10b10-uint-pack32 #x3E)
  (:a2r10g10b10-sint-pack32 #x3F)
  (:a2b10g10r10-unorm-pack32 #x40)
  (:a2b10g10r10-snorm-pack32 #x41)
  (:a2b10g10r10-uscaled-pack32 #x42)
  (:a2b10g10r10-sscaled-pack32 #x43)
  (:a2b10g10r10-uint-pack32 #x44)
  (:a2b10g10r10-sint-pack32 #x45)
  (:r16-unorm #x46)
  (:r16-snorm #x47)
  (:r16-uscaled #x48)
  (:r16-sscaled #x49)
  (:r16-uint #x4A)
  (:r16-sint #x4B)
  (:r16-sfloat #x4C)
  (:r16g16-unorm #x4D)
  (:r16g16-snorm #x4E)
  (:r16g16-uscaled #x4F)
  (:r16g16-sscaled #x50)
  (:r16g16-uint #x51)
  (:r16g16-sint #x52)
  (:r16g16-sfloat #x53)
  (:r16g16b16-unorm #x54)
  (:r16g16b16-snorm #x55)
  (:r16g16b16-uscaled #x56)
  (:r16g16b16-sscaled #x57)
  (:r16g16b16-uint #x58)
  (:r16g16b16-sint #x59)
  (:r16g16b16-sfloat #x5A)
  (:r16g16b16a16-unorm #x5B)
  (:r16g16b16a16-snorm #x5C)
  (:r16g16b16a16-uscaled #x5D)
  (:r16g16b16a16-sscaled #x5E)
  (:r16g16b16a16-uint #x5F)
  (:r16g16b16a16-sint #x60)
  (:r16g16b16a16-sfloat #x61)
  (:r32-uint #x62)
  (:r32-sint #x63)
  (:r32-sfloat #x64)
  (:r32g32-uint #x65)
  (:r32g32-sint #x66)
  (:r32g32-sfloat #x67)
  (:r32g32b32-uint #x68)
  (:r32g32b32-sint #x69)
  (:r32g32b32-sfloat #x6A)
  (:r32g32b32a32-uint #x6B)
  (:r32g32b32a32-sint #x6C)
  (:r32g32b32a32-sfloat #x6D)
  (:r64-uint #x6E)
  (:r64-sint #x6F)
  (:r64-sfloat #x70)
  (:r64g64-uint #x71)
  (:r64g64-sint #x72)
  (:r64g64-sfloat #x73)
  (:r64g64b64-uint #x74)
  (:r64g64b64-sint #x75)
  (:r64g64b64-sfloat #x76)
  (:r64g64b64a64-uint #x77)
  (:r64g64b64a64-sint #x78)
  (:r64g64b64a64-sfloat #x79)
  (:b10g11r11-ufloat-pack32 #x7A)
  (:e5b9g9r9-ufloat-pack32 #x7B)
  (:d16-unorm #x7C)
  (:x8-d24-unorm-pack32 #x7D)
  (:d32-sfloat #x7E)
  (:s8-uint #x7F)
  (:d16-unorm-s8-uint #x80)
  (:d24-unorm-s8-uint #x81)
  (:d32-sfloat-s8-uint #x82)
  (:bc1-rgb-unorm-block #x83)
  (:bc1-rgb-srgb-block #x84)
  (:bc1-rgba-unorm-block #x85)
  (:bc1-rgba-srgb-block #x86)
  (:bc2-unorm-block #x87)
  (:bc2-srgb-block #x88)
  (:bc3-unorm-block #x89)
  (:bc3-srgb-block #x8A)
  (:bc4-unorm-block #x8B)
  (:bc4-snorm-block #x8C)
  (:bc5-unorm-block #x8D)
  (:bc5-snorm-block #x8E)
  (:bc6h-ufloat-block #x8F)
  (:bc6h-sfloat-block #x90)
  (:bc7-unorm-block #x91)
  (:bc7-srgb-block #x92)
  (:etc2-r8g8b8-unorm-block #x93)
  (:etc2-r8g8b8-srgb-block #x94)
  (:etc2-r8g8b8a1-unorm-block #x95)
  (:etc2-r8g8b8a1-srgb-block #x96)
  (:etc2-r8g8b8a8-unorm-block #x97)
  (:etc2-r8g8b8a8-srgb-block #x98)
  (:eac-r11-unorm-block #x99)
  (:eac-r11-snorm-block #x9A)
  (:eac-r11g11-unorm-block #x9B)
  (:eac-r11g11-snorm-block #x9C)
  (:astc-4x4-unorm-block #x9D)
  (:astc-4x4-srgb-block #x9E)
  (:astc-5x4-unorm-block #x9F)
  (:astc-5x4-srgb-block #xA0)
  (:astc-5x5-unorm-block #xA1)
  (:astc-5x5-srgb-block #xA2)
  (:astc-6x5-unorm-block #xA3)
  (:astc-6x5-srgb-block #xA4)
  (:astc-6x6-unorm-block #xA5)
  (:astc-6x6-srgb-block #xA6)
  (:astc-8x5-unorm-block #xA7)
  (:astc-8x5-srgb-block #xA8)
  (:astc-8x6-unorm-block #xA9)
  (:astc-8x6-srgb-block #xAA)
  (:astc-8x8-unorm-block #xAB)
  (:astc-8x8-srgb-block #xAC)
  (:astc-10x5-unorm-block #xAD)
  (:astc-10x5-srgb-block #xAE)
  (:astc-10x6-unorm-block #xAF)
  (:astc-10x6-srgb-block #xB0)
  (:astc-10x8-unorm-block #xB1)
  (:astc-10x8-srgb-block #xB2)
  (:astc-10x10-unorm-block #xB3)
  (:astc-10x10-srgb-block #xB4)
  (:astc-12x10-unorm-block #xB5)
  (:astc-12x10-srgb-block #xB6)
  (:astc-12x12-unorm-block #xB7)
  (:astc-12x12-srgb-block #xB8))

(defcenum (front-face)
  (:counter-clockwise #x0)
  (:clockwise #x1))

(defcenum (image-layout)
  (:undefined #x0) ;; Implicit layout an image is when its contents are undefined due to various reasons (e.g. right after creation)
  (:general #x1) ;; General layout when image can be used for any kind of access
  (:color-attachment-optimal #x2) ;; Optimal layout when image is only used for color attachment read/write
  (:depth-stencil-attachment-optimal #x3) ;; Optimal layout when image is only used for depth/stencil attachment read/write
  (:depth-stencil-read-only-optimal #x4) ;; Optimal layout when image is used for read only depth/stencil attachment and shader access
  (:shader-read-only-optimal #x5) ;; Optimal layout when image is used for read only shader access
  (:transfer-src-optimal #x6) ;; Optimal layout when image is used only as source of transfer operations
  (:transfer-dst-optimal #x7) ;; Optimal layout when image is used only as destination of transfer operations
  (:preinitialized #x8)) ;; Initial layout used when the data is populated by the CPU

(defcenum (image-tiling)
  (:optimal #x0)
  (:linear #x1))

(defcenum (image-type)
  (:1d #x0)
  (:2d #x1)
  (:3d #x2))

(defcenum (image-view-type)
  (:1d #x0)
  (:2d #x1)
  (:3d #x2)
  (:cube #x3)
  (:1d-array #x4)
  (:2d-array #x5)
  (:cube-array #x6))

(defcenum (index-type)
  (:uint16 #x0)
  (:uint32 #x1))

(defcenum (internal-allocation-type)
  (:executable #x0))

(defcenum (logic-op)
  (:clear #x0)
  (:and #x1)
  (:and-reverse #x2)
  (:copy #x3)
  (:and-inverted #x4)
  (:no-op #x5)
  (:xor #x6)
  (:or #x7)
  (:nor #x8)
  (:equivalent #x9)
  (:invert #xA)
  (:or-reverse #xB)
  (:copy-inverted #xC)
  (:or-inverted #xD)
  (:nand #xE)
  (:set #xF))

(defcenum (physical-device-type)
  (:other #x0)
  (:integrated-gpu #x1)
  (:discrete-gpu #x2)
  (:virtual-gpu #x3)
  (:cpu #x4))

(defcenum (pipeline-bind-point)
  (:graphics #x0)
  (:compute #x1))

(defcenum (pipeline-cache-header-version)
  (:one #x1))

(defcenum (polygon-mode)
  (:fill #x0)
  (:line #x1)
  (:point #x2))

(defcenum (present-mode-khr)
  (:immediate-khr #x0)
  (:mailbox-khr #x1)
  (:fifo-khr #x2)
  (:fifo-relaxed-khr #x3))

(defcenum (primitive-topology)
  (:point-list #x0)
  (:line-list #x1)
  (:line-strip #x2)
  (:triangle-list #x3)
  (:triangle-strip #x4)
  (:triangle-fan #x5)
  (:line-list-with-adjacency #x6)
  (:line-strip-with-adjacency #x7)
  (:triangle-list-with-adjacency #x8)
  (:triangle-strip-with-adjacency #x9)
  (:patch-list #xA))

(defcenum (query-type)
  (:occlusion #x0)
  (:pipeline-statistics #x1) ;; Optional
  (:timestamp #x2))

(defcenum (result)
  (:success #x0) ;; Command completed successfully
  (:not-ready #x1) ;; A fence or query has not yet completed
  (:timeout #x2) ;; A wait operation has not completed in the specified time
  (:event-set #x3) ;; An event is signaled
  (:event-reset #x4) ;; An event is unsignalled
  (:incomplete #x5) ;; A return array was too small for the resul
  (:error-out-of-host-memory -1) ;; A host memory allocation has failed
  (:error-out-of-device-memory -2) ;; A device memory allocation has failed
  (:error-initialization-failed -3) ;; The logical device has been lost. See <<devsandqueues-lost-device>>
  (:error-device-lost -4) ;; Initialization of a object has failed
  (:error-memory-map-failed -5) ;; Mapping of a memory object has failed
  (:error-layer-not-present -6) ;; Layer specified does not exist
  (:error-extension-not-present -7) ;; Extension specified does not exist
  (:error-feature-not-present -8) ;; Requested feature is not available on this device
  (:error-incompatible-driver -9) ;; Unable to find a Vulkan driver
  (:error-too-many-objects -10) ;; Too many objects of the type have already been created
  (:error-format-not-supported -11)) ;; Requested format is not supported on this device

(defcenum (sampler-address-mode)
  (:repeat #x0)
  (:mirrored-repeat #x1)
  (:clamp-to-edge #x2)
  (:clamp-to-border #x3)
  (:mirror-clamp-to-edge #x4))

(defcenum (sampler-mipmap-mode)
  (:nearest #x0) ;; Choose nearest mip level
  (:linear #x1)) ;; Linear filter between mip levels

(defcenum (sharing-mode)
  (:exclusive #x0)
  (:concurrent #x1))

(defcenum (stencil-op)
  (:keep #x0)
  (:zero #x1)
  (:replace #x2)
  (:increment-and-clamp #x3)
  (:decrement-and-clamp #x4)
  (:invert #x5)
  (:increment-and-wrap #x6)
  (:decrement-and-wrap #x7))

(defcenum (structure-type)
  (:application-info #x0)
  (:instance-create-info #x1)
  (:device-queue-create-info #x2)
  (:device-create-info #x3)
  (:submit-info #x4)
  (:memory-allocate-info #x5)
  (:mapped-memory-range #x6)
  (:bind-sparse-info #x7)
  (:fence-create-info #x8)
  (:semaphore-create-info #x9)
  (:event-create-info #xA)
  (:query-pool-create-info #xB)
  (:buffer-create-info #xC)
  (:buffer-view-create-info #xD)
  (:image-create-info #xE)
  (:image-view-create-info #xF)
  (:shader-module-create-info #x10)
  (:pipeline-cache-create-info #x11)
  (:pipeline-shader-stage-create-info #x12)
  (:pipeline-vertex-input-state-create-info #x13)
  (:pipeline-input-assembly-state-create-info #x14)
  (:pipeline-tessellation-state-create-info #x15)
  (:pipeline-viewport-state-create-info #x16)
  (:pipeline-rasterization-state-create-info #x17)
  (:pipeline-multisample-state-create-info #x18)
  (:pipeline-depth-stencil-state-create-info #x19)
  (:pipeline-color-blend-state-create-info #x1A)
  (:pipeline-dynamic-state-create-info #x1B)
  (:graphics-pipeline-create-info #x1C)
  (:compute-pipeline-create-info #x1D)
  (:pipeline-layout-create-info #x1E)
  (:sampler-create-info #x1F)
  (:descriptor-set-layout-create-info #x20)
  (:descriptor-pool-create-info #x21)
  (:descriptor-set-allocate-info #x22)
  (:write-descriptor-set #x23)
  (:copy-descriptor-set #x24)
  (:framebuffer-create-info #x25)
  (:render-pass-create-info #x26)
  (:command-pool-create-info #x27)
  (:command-buffer-allocate-info #x28)
  (:command-buffer-inheritance-info #x29)
  (:command-buffer-begin-info #x2A)
  (:render-pass-begin-info #x2B)
  (:buffer-memory-barrier #x2C)
  (:image-memory-barrier #x2D)
  (:memory-barrier #x2E)
  (:loader-instance-create-info #x2F)
  (:loader-device-create-info #x30))

(defcenum (subpass-contents)
  (:inline #x0)
  (:secondary-command-buffers #x1))

(defcenum (system-allocation-scope)
  (:command #x0)
  (:object #x1)
  (:cache #x2)
  (:device #x3)
  (:instance #x4))

(defcenum (vertex-input-rate)
  (:vertex #x0)
  (:instance #x1))

