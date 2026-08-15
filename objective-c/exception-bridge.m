#import <Foundation/Foundation.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  char *name;
  char *reason;
  char *call_stack;
} LuvObjectiveCFailure;

enum {
  LUV_OBJECTIVE_C_RETURNED = 0,
  LUV_OBJECTIVE_C_EXCEPTION = 1,
  LUV_OBJECTIVE_C_BRIDGE_ERROR = 2,
};

static char *luv_copy_string(NSString *string) {
  if (string == nil) {
    return NULL;
  }

  const char *utf8 = [string UTF8String];
  return utf8 == NULL ? NULL : strdup(utf8);
}

static void luv_fill_failure(LuvObjectiveCFailure *failure, NSString *name,
                             NSString *reason, NSString *call_stack) {
  failure->name = luv_copy_string(name);
  failure->reason = luv_copy_string(reason);
  failure->call_stack = luv_copy_string(call_stack);
}

static int luv_bridge_error(LuvObjectiveCFailure *failure, NSString *reason) {
  luv_fill_failure(failure, @"LuvObjectiveCBridgeError", reason, nil);
  return LUV_OBJECTIVE_C_BRIDGE_ERROR;
}

int luv_objc_invoke(void *receiver_pointer, void *selector_pointer,
                    void *return_value, size_t return_size,
                    size_t argument_count, void *const *arguments,
                    const size_t *argument_sizes,
                    LuvObjectiveCFailure *failure) {
  NSInvocation *invocation = nil;
  failure->name = NULL;
  failure->reason = NULL;
  failure->call_stack = NULL;

  @try {
    id receiver = (__bridge id)receiver_pointer;
    SEL selector = (SEL)selector_pointer;
    if (receiver == nil) {
      return luv_bridge_error(failure,
                              @"Cannot send a declared message to nil.");
    }

    NSMethodSignature *signature =
        [receiver methodSignatureForSelector:selector];
    if (signature == nil) {
      return luv_bridge_error(
          failure,
          [NSString
              stringWithFormat:@"Receiver does not implement selector %@.",
                               NSStringFromSelector(selector)]);
    }

    NSUInteger native_argument_count = [signature numberOfArguments];
    if (native_argument_count != argument_count + 2) {
      return luv_bridge_error(
          failure,
          [NSString
              stringWithFormat:@"Selector %@ expects %lu explicit arguments, "
                                "but the declaration supplies %zu.",
                               NSStringFromSelector(selector),
                               (unsigned long)(native_argument_count - 2),
                               argument_count]);
    }

    if ([signature methodReturnLength] != return_size) {
      return luv_bridge_error(
          failure,
          [NSString
              stringWithFormat:@"Selector %@ has a %lu-byte result, but "
                                "the declaration supplies %zu bytes.",
                               NSStringFromSelector(selector),
                               (unsigned long)[signature methodReturnLength],
                               return_size]);
    }

    for (size_t index = 0; index < argument_count; ++index) {
      NSUInteger native_size = 0;
      NSGetSizeAndAlignment(
          [signature getArgumentTypeAtIndex:(NSUInteger)index + 2],
          &native_size, NULL);
      if (native_size != argument_sizes[index]) {
        return luv_bridge_error(
            failure,
            [NSString
                stringWithFormat:@"Argument %zu of selector %@ is %lu bytes, "
                                  "but the declaration supplies %zu bytes.",
                                 index, NSStringFromSelector(selector),
                                 (unsigned long)native_size,
                                 argument_sizes[index]]);
      }
    }

    invocation =
        [[NSInvocation invocationWithMethodSignature:signature] retain];
    [invocation setTarget:receiver];
    [invocation setSelector:selector];
    for (size_t index = 0; index < argument_count; ++index) {
      [invocation setArgument:arguments[index] atIndex:(NSInteger)index + 2];
    }

    [invocation invoke];
    if (return_size != 0) {
      [invocation getReturnValue:return_value];
    }
    [invocation release];
    return LUV_OBJECTIVE_C_RETURNED;
  } @catch (NSException *exception) {
    @try {
      NSString *call_stack =
          [[exception callStackSymbols] componentsJoinedByString:@"\n"];
      luv_fill_failure(failure, [exception name], [exception reason],
                       call_stack);
    } @catch (NSException *description_exception) {
      (void)description_exception;
      failure->name = strdup("NSException");
      failure->reason =
          strdup("An Objective-C exception was caught, but describing it "
                 "raised another exception.");
    }
    [invocation release];
    return LUV_OBJECTIVE_C_EXCEPTION;
  }
}

void luv_objc_failure_free(LuvObjectiveCFailure *failure) {
  free(failure->name);
  free(failure->reason);
  free(failure->call_stack);
  failure->name = NULL;
  failure->reason = NULL;
  failure->call_stack = NULL;
}
