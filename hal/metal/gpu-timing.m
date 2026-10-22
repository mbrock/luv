// Metal 4 reports each commit's GPU start and end through a feedback block.
// Blocks cannot be made from Lisp, so this helper attaches one that writes
// the times into a ring the Lisp side owns and polls.  It reaches Metal 4
// through the Objective-C runtime, like the rest of Luv, so it needs no SDK
// newer than the one that compiles it.

#import <Foundation/Foundation.h>
#import <objc/message.h>
#import <objc/runtime.h>

#include <Block.h>
#include <stdint.h>

typedef struct {
  uint64_t value;
  double start;
  double end;
} LuvMetalGpuTime;

static id luv_send(id receiver, const char *selector) {
  return ((id(*)(id, SEL))objc_msgSend)(receiver, sel_registerName(selector));
}

static double luv_send_double(id receiver, const char *selector) {
  return ((double (*)(id, SEL))objc_msgSend)(receiver,
                                             sel_registerName(selector));
}

// Return +1 MTL4CommitOptions whose feedback records submission VALUE in
// RING, or NULL when Metal 4 is unavailable.
void *luv_metal_timed_commit_options(LuvMetalGpuTime *ring, uint32_t capacity,
                                     uint64_t value) {
  Class class = objc_getClass("MTL4CommitOptions");
  if (class == Nil || ring == NULL || capacity == 0) {
    return NULL;
  }
  id options = luv_send(luv_send((id)class, "alloc"), "init");
  LuvMetalGpuTime *slot = &ring[value % capacity];
  void (^handler)(id) = Block_copy(^(id feedback) {
    slot->start = luv_send_double(feedback, "GPUStartTime");
    slot->end = luv_send_double(feedback, "GPUEndTime");
    __atomic_store_n(&slot->value, value, __ATOMIC_RELEASE);
  });
  ((void (*)(id, SEL, id))objc_msgSend)(
      options, sel_registerName("addFeedbackHandler:"), (id)handler);
  Block_release(handler);
  return options;
}

// Store submission VALUE's GPU seconds in SECONDS and return 1 once its
// feedback has arrived, or return 0.
int luv_metal_gpu_time(LuvMetalGpuTime *ring, uint32_t capacity,
                       uint64_t value, double *seconds) {
  LuvMetalGpuTime *slot = &ring[value % capacity];
  if (__atomic_load_n(&slot->value, __ATOMIC_ACQUIRE) != value) {
    return 0;
  }
  *seconds = slot->end - slot->start;
  return 1;
}

void luv_metal_release(void *object) { [(id)object release]; }
