// What a program's shaders declare, as luv-shaderc writes it into each
// generated NAME.hh (see docs/nhal.md). NHAL builds root signatures and
// binding tables from this rather than from the shader binaries.
#ifndef MOPPE_NHAL_REFLECTION_HH
#define MOPPE_NHAL_REFLECTION_HH

#include <cstdint>
#include <span>

namespace moppe::nhal {
  enum class ResourceKind : std::uint8_t {
    uniform_block,
    storage_buffer,
    read_write_storage_buffer,
    texture_2d,
    depth_texture_2d,
    uint_texture_2d,
    texture_2d_array,
    depth_texture_2d_array,
    texture_cube,
    texture_3d,
    read_write_texture_2d,
    sampler,
    comparison_sampler,
  };

  using StageMask = std::uint8_t;
  inline constexpr StageMask stage_vertex = 1;
  inline constexpr StageMask stage_fragment = 2;
  inline constexpr StageMask stage_compute = 4;

  // Buffer kinds share one binding space; textures and samplers each have
  // their own. Sizes are bytes for uniform blocks and zero otherwise.
  struct Resource {
    const char* name;
    ResourceKind kind;
    std::uint32_t binding;
    StageMask stages;
    std::uint32_t size;
  };

  struct Program {
    const char* name;
    const char* vertex_entry;
    const char* fragment_entry;
    const char* compute_entry;
    std::span<const Resource> resources;
    std::uint32_t color_outputs;
    // Compute programs: threads per workgroup.
    std::uint32_t workgroup_size[3] = { 0, 0, 0 };
  };

  constexpr bool is_buffer (ResourceKind kind) {
    return kind == ResourceKind::uniform_block
      || kind == ResourceKind::storage_buffer
      || kind == ResourceKind::read_write_storage_buffer;
  }

  // Sampled and read-only textures share the texture binding family.
  constexpr bool is_texture (ResourceKind kind) {
    return kind == ResourceKind::texture_2d
      || kind == ResourceKind::depth_texture_2d
      || kind == ResourceKind::uint_texture_2d
      || kind == ResourceKind::texture_2d_array
      || kind == ResourceKind::depth_texture_2d_array
      || kind == ResourceKind::texture_cube
      || kind == ResourceKind::texture_3d;
  }

  // Storage textures are their own binding family: Metal texture(16 + i)
  // with access::read_write, past the sampled textures' range, and HLSL
  // u-registers in space 1 (buffers' UAVs are u-registers in space 0).
  constexpr bool is_storage_texture (ResourceKind kind) {
    return kind == ResourceKind::read_write_texture_2d;
  }

  constexpr bool is_sampler (ResourceKind kind) {
    return kind == ResourceKind::sampler
      || kind == ResourceKind::comparison_sampler;
  }
}

#endif
