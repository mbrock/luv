(defpackage #:luft.benchmark.tests
  (:use #:cl #:rove)
  (:local-nicknames (#:benchmark #:luft.benchmark)
                    (#:render #:luft.render)))

(in-package #:luft.benchmark.tests)

(deftest dense-and-chain-occupancy-produce-identical-records
  (let* ((case (benchmark::make-mesher-case 4 :terrain))
         (surface (benchmark::mesher-case-surface case))
         (chain
           (render:make-face-materialization-from-surface
            surface (benchmark::mesher-case-chain-occupancy case)))
         (dense
           (render:make-face-materialization-from-surface
            surface (benchmark::mesher-case-dense-occupancy case))))
    (ok (equalp (render:face-materialization-words chain)
                (render:face-materialization-words dense)))
    (ok (= (render:face-materialization-positive-count chain)
           (render:face-materialization-positive-count dense)))
    (ok (= (render:face-materialization-negative-count chain)
           (render:face-materialization-negative-count dense)))))

(deftest one-sample-retains-work-and-runtime-evidence
  (let* ((case (benchmark::make-mesher-case 4 :architecture))
         (samples
           (benchmark::measure-benchmark-phase
            case :full-chain 1 0 (make-broadcast-stream)))
         (sample (aref samples 0)))
    (ok (= 1 (length samples)))
    (ok (plusp (benchmark::mesher-sample-cell-count sample)))
    (ok (plusp (benchmark::mesher-sample-face-count sample)))
    (ok (>= (benchmark::mesher-sample-elapsed-seconds sample) 0d0))
    (ok (>= (benchmark::mesher-sample-bytes-consed sample) 0))))
