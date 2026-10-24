// Execute shaderc's scalar/vector MOD regression on Metal when available.
import Foundation
import Metal

guard let device = MTLCreateSystemDefaultDevice() else { exit(77) }
let source = try String(contentsOfFile: CommandLine.arguments[1], encoding: .utf8)
let library = try device.makeLibrary(source: source, options: nil)
let pipeline = try device.makeComputePipelineState(
    function: library.makeFunction(name: "signed_mod_compute")!)
let edges: [Int32] = [.min, .min + 1, -2000000000, -7, -4, -1,
                     0, 1, 4, 7, 2000000000, .max - 1, .max]
var pairs: [SIMD2<Int32>] = []
for a in edges {
    // Division by zero and INT_MIN / -1 are outside the DSL's contract.
    for b in edges where b != 0 && !(a == .min && b == -1) {
        pairs.append(SIMD2(a, b))
    }
}
let input = device.makeBuffer(bytes: pairs, length: pairs.count * 8,
                              options: .storageModeShared)!
let output = device.makeBuffer(length: pairs.count * 16, options: .storageModeShared)!
let command = device.makeCommandQueue()!.makeCommandBuffer()!
let encoder = command.makeComputeCommandEncoder()!
encoder.setComputePipelineState(pipeline)
encoder.setBuffer(input, offset: 0, index: 0)
encoder.setBuffer(output, offset: 0, index: 1)
encoder.dispatchThreads(MTLSize(width: pairs.count, height: 1, depth: 1),
                        threadsPerThreadgroup: MTLSize(width: 1, height: 1, depth: 1))
encoder.endEncoding()
command.commit()
command.waitUntilCompleted()
if let error = command.error { throw error }
let values = output.contents().bindMemory(to: SIMD4<Int32>.self, capacity: pairs.count)
var failures = 0
for (i, pair) in pairs.enumerated() {
    let a = Int64(pair.x), b = Int64(pair.y)
    // A wide host reference: this formula cannot overflow for Int32 inputs.
    let mod = Int32(((a % b) + b) % b)
    let c: Int64 = b > 0 ? -7 : 7
    let expected = SIMD4(mod, Int32(a % b), mod, Int32(((b % c) + c) % c))
    if values[i] != expected {
        print("\(pair): got \(values[i]), expected \(expected)")
        failures += 1
    }
}
print("\(device.name): \(pairs.count) scalar/vector MOD cases, \(failures) failures")
exit(failures == 0 ? 0 : 1)
