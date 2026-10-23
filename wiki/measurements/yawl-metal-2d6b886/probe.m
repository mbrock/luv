#import <Foundation/Foundation.h>
#import <Metal/Metal.h>

void probeDevice(void *pointer) {
    id<MTLDevice> device = (__bridge id<MTLDevice>)pointer;
    NSLog(@"DEVICE %@ counterSets=%@", device.name, device.counterSets);
    for (id<MTLCounterSet> set in device.counterSets) {
        NSLog(@"COUNTER SET %@", set.name);
        for (id<MTLCounter> counter in set.counters) NSLog(@"COUNTER %@", counter.name);
    }
    NSLog(@"CAPTURE supported=%d", [[MTLCaptureManager sharedCaptureManager] supportsDestination:MTLCaptureDestinationGPUTraceDocument]);
}

int startCapture(void *pointer, const char *path) {
    MTLCaptureDescriptor *descriptor = [MTLCaptureDescriptor new];
    descriptor.captureObject = (__bridge id)pointer;
    descriptor.destination = MTLCaptureDestinationGPUTraceDocument;
    descriptor.outputURL = [NSURL fileURLWithPath:[NSString stringWithUTF8String:path]];
    NSError *error = nil;
    BOOL ok = [[MTLCaptureManager sharedCaptureManager] startCaptureWithDescriptor:descriptor error:&error];
    NSLog(@"CAPTURE start=%d error=%@", ok, error);
    return ok;
}

void stopCapture(void) { [[MTLCaptureManager sharedCaptureManager] stopCapture]; }

void labelEncoder(void *pointer, const char *label) {
    id<MTL4CommandEncoder> encoder = (__bridge id<MTL4CommandEncoder>)pointer;
    encoder.label = [NSString stringWithUTF8String:label];
}
