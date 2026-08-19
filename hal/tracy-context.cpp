#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "tracy/TracyC.h"

namespace {

std::vector<TracyCZoneCtx>& zone_contexts()
{
    thread_local std::vector<TracyCZoneCtx> contexts;
    if( contexts.capacity() == 0 ) contexts.reserve( 64 );
    return contexts;
}

}

extern "C" {

TRACY_API void luv_tracy_emit_zone_begin(
    const struct ___tracy_source_location_data* source_location,
    int32_t active )
{
    zone_contexts().push_back(
        ___tracy_emit_zone_begin( source_location, active ) );
}

TRACY_API void luv_tracy_emit_zone_value( uint64_t value )
{
    auto& contexts = zone_contexts();
    assert( !contexts.empty() );
    ___tracy_emit_zone_value( contexts.back(), value );
}

TRACY_API void luv_tracy_emit_zone_end()
{
    auto& contexts = zone_contexts();
    assert( !contexts.empty() );
    const auto context = contexts.back();
    contexts.pop_back();
    ___tracy_emit_zone_end( context );
}

TRACY_API size_t luv_tracy_zone_depth()
{
    return zone_contexts().size();
}

}
