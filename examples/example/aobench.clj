(ns example.aobench
  (:gen-class)
  (:require [jise.core :refer [defclass]])
  (:import [java.io FileOutputStream]
           [java.util Random]))

^:public
(defclass Vec
  ^:public ^double (def x)
  ^:public ^double (def y)
  ^:public ^double (def z)

  ^:public
  (defm Vec []
    (this 0.0 0.0 0.0))

  ^:public
  (defm Vec [^double x ^double y ^double z]
    (set! (.-x this) x)
    (set! (.-y this) y)
    (set! (.-z this) z))

  ^:public ^double
  (defm dot [^Vec v]
    (+ (* x (.-x v)) (* y (.-y v)) (* z (.-z v))))

  ^:public ^:static ^Vec
  (defm cross [^Vec v0 ^Vec v1]
    (Vec. (- (* (.-y v0) (.-z v1)) (* (.-z v0) (.-y v1)))
          (- (* (.-z v0) (.-x v1)) (* (.-x v0) (.-z v1)))
          (- (* (.-x v0) (.-y v1)) (* (.-y v0) (.-x v1)))))

  ^:public ^Vec
  (defm normalize! []
    (let [len (Math/sqrt (.dot this this))]
      (when (> len 1.0e-17)
        (set! (.-x this) (/ (.-x this) len))
        (set! (.-y this) (/ (.-y this) len))
        (set! (.-z this) (/ (.-z this) len))))
    this)

  ^:public ^Vec
  (defm copy []
    (Vec. x y z)))

^:public
(defclass Isect
  ^:public ^double (def t)
  ^:public ^Vec (def p)
  ^:public ^Vec (def n)
  ^:public ^boolean (def hit?)

  ^:public
  (defm Isect [^double t]
    (set! (.-t this) t)
    (set! p (Vec.))
    (set! n (Vec.))
    (set! hit? false)))

^:public
(defclass Sphere
  ^:public ^:final
  (def ^Vec center)
  ^:public ^:final
  (def ^double radius)

  ^:public
  (defm Sphere [^Vec center ^double radius]
    (set! (.-center this) center)
    (set! (.-radius this) radius)))

^:public
(defclass Plane
  ^:public ^:final
  (def ^Vec p)
  ^:public ^:final
  (def ^Vec n)

  ^:public
  (defm Plane [^Vec p ^Vec n]
    (set! (.-p this) p)
    (set! (.-n this) n)))

^:public
(defclass Ray
  ^:public ^:final
  (def ^Vec org)
  ^:public ^:final
  (def ^Vec dir)

  ^:public
  (defm Ray [^Vec org ^Vec dir]
    (set! (.-org this) org)
    (set! (.-dir this) dir)))

^:public
(defclass AOBench
  ^:static ^:private ^:final ^int (def NAO_SAMPLES 8)

  ^:private ^int (def width)
  ^:private ^int (def height)
  ^:private ^int (def nsubsamples)

  ^:private ^bytes (def image)

  ^:private ^{:tag [Sphere]} (def spheres)
  ^:private ^Plane (def plane)

  ^:private ^Random (def random (Random. (long 0)))

  ^:public
  (defm AOBench [^int width ^int height ^int nsubsamples]
    (super)
    (set! (.-width this) width)
    (set! (.-height this) height)
    (set! (.-nsubsamples this) nsubsamples)
    (set! image (new [byte] (* width height 3))))

  ^:public ^AOBench
  (defm spheres [^{:tag [Sphere]} spheres]
    (set! (.-spheres this) spheres)
    this)

  ^:public ^AOBench
  (defm plane [^Plane plane]
    (set! (.-plane this) plane)
    this)

  ^:private
  (defm ray-sphere-intersect [^Isect isect ^Ray ray ^Sphere sphere]
    (let [rs (Vec. (- (.. ray -org -x) (.. sphere -center -x))
                   (- (.. ray -org -y) (.. sphere -center -y))
                   (- (.. ray -org -z) (.. sphere -center -z)))
          B (.dot rs (.-dir ray))
          C (- (.dot rs rs) (* (.-radius sphere) (.-radius sphere)))
          D (- (* B B) C)]
      (when (> D 0)
        (let [t (- (- B) (Math/sqrt D))]
          (when (and (> t 0) (< t (.-t isect)))
            (set! (.-t isect) t)
            (set! (.-hit? isect) true)
            (set! (.. isect -p -x) (+ (.. ray -org -x) (* (.. ray -dir -x) t)))
            (set! (.. isect -p -y) (+ (.. ray -org -y) (* (.. ray -dir -y) t)))
            (set! (.. isect -p -z) (+ (.. ray -org -z) (* (.. ray -dir -z) t)))
            (set! (.. isect -n -x) (- (.. isect -p -x) (.. sphere -center -x)))
            (set! (.. isect -n -y) (- (.. isect -p -y) (.. sphere -center -y)))
            (set! (.. isect -n -z) (- (.. isect -p -z) (.. sphere -center -z)))
            (.normalize! (.-n isect)))))))

  ^:private
  (defm ray-plane-intersect [^Isect isect ^Ray ray ^Plane plane]
    (let [d (- (.dot (.-p plane) (.-n plane)))
          v (.dot (.-dir ray) (.-n plane))]
      (when-not (< (Math/abs v) 1.0e-17)
        (let [t (/ (- (+ (.dot (.-org ray) (.-n plane)) d)) v)]
          (when (and (> t 0) (< t (.-t isect)))
            (set! (.-t isect) t)
            (set! (.-hit? isect) true)
            (set! (.. isect -p -x) (+ (.. ray -org -x) (* (.. ray -dir -x) t)))
            (set! (.. isect -p -y) (+ (.. ray -org -y) (* (.. ray -dir -y) t)))
            (set! (.. isect -p -z) (+ (.. ray -org -z) (* (.. ray -dir -z) t)))
            (set! (.-n isect) (.copy (.-n plane))))))))

  ^:private ^{:tag [Vec]}
  (defm ortho-basis [^Vec n]
    (let [v1 (cond (< -0.6 (.-x n) 0.6) (Vec. 1.0 0.0 0.0)
                   (< -0.6 (.-y n) 0.6) (Vec. 0.0 1.0 0.0)
                   (< -0.6 (.-z n) 0.6) (Vec. 0.0 0.0 1.0)
                   true                 (Vec. 1.0 0.0 0.0))
          v0 (.normalize! (Vec/cross v1 n))]
      (new [Vec] [v0 (.normalize! (Vec/cross n v0)) (.copy n)])))

  ^:private ^Vec
  (defm ambient-occlusion [^Isect isect]
    (let [ntheta NAO_SAMPLES
          nphi NAO_SAMPLES
          eps 0.0001
          p (Vec. (+ (.. isect -p -x) (* eps (.. isect -n -x)))
                  (+ (.. isect -p -y) (* eps (.. isect -n -y)))
                  (+ (.. isect -p -z) (* eps (.. isect -n -z))))
          basis (ortho-basis (.-n isect))
          occlusion 0.0]
      (for [j 0, (< j ntheta), (inc! j)]
        (for [i 0, (< i nphi), (inc! i)]
          (let [theta (Math/sqrt (.nextDouble random))
                phi (* 2.0 Math/PI (.nextDouble random))
                x (* (Math/cos phi) theta)
                y (* (Math/sin phi) theta)
                z (Math/sqrt (- 1.0 (* theta theta)))
                rx (+ (* x (.-x (basis 0))) (* y (.-x (basis 1))) (* z (.-x (basis 2))))
                ry (+ (* x (.-y (basis 0))) (* y (.-y (basis 1))) (* z (.-y (basis 2))))
                rz (+ (* x (.-z (basis 0))) (* y (.-z (basis 1))) (* z (.-z (basis 2))))
                ray (Ray. p (Vec. rx ry rz))
                occ-isect (Isect. 1.0e+17)]
            (ray-sphere-intersect occ-isect ray (spheres 0))
            (ray-sphere-intersect occ-isect ray (spheres 1))
            (ray-sphere-intersect occ-isect ray (spheres 2))
            (ray-plane-intersect occ-isect ray plane)
            (when (.-hit? occ-isect)
              (inc! occlusion)))))
      (set! occlusion (/ (- (* ntheta nphi) occlusion) (* ntheta nphi)))
      (Vec. occlusion occlusion occlusion)))

  ^:private ^int
  (defm clamp [^double f]
    (let [^int n (* f 255.5)]
      (cond (< n 0)   0
            (> n 255) 255
            true      n)))

  ^:public ^bytes
  (defm render []
    (let [fimg (new [double] (* width height 3))
          ^double nsub*nsub (* nsubsamples nsubsamples)]
      (for [y 0, (< y height), (inc! y)]
        (for [x 0, (< x width), (inc! x)]
          (let [offset (* 3 (+ (* y width) x))]
            (for [v 0, (< v nsubsamples), (inc! v)]
              (for [u 0, (< u nsubsamples), (inc! u)]
                (let [px (/ (- (+ x (/ u (double nsubsamples))) (/ width 2.0)) (/ width 2.0))
                      py (/ (- (- (+ y (/ v (double nsubsamples))) (/ height 2.0))) (/ height 2.0))
                      ray (Ray. (Vec.) (.normalize! (Vec. px py -1.0)))
                      isect (Isect. 1.0e+17)]
                  (ray-sphere-intersect isect ray (spheres 0))
                  (ray-sphere-intersect isect ray (spheres 1))
                  (ray-sphere-intersect isect ray (spheres 2))
                  (ray-plane-intersect isect ray plane)
                  (when (.-hit? isect)
                    (let [col (ambient-occlusion isect)]
                      (inc! (fimg (+ offset 0)) (.-x col))
                      (inc! (fimg (+ offset 1)) (.-y col))
                      (inc! (fimg (+ offset 2)) (.-z col)))))))
            (set! (fimg (+ offset 0)) (/ (fimg (+ offset 0)) nsub*nsub))
            (set! (fimg (+ offset 1)) (/ (fimg (+ offset 1)) nsub*nsub))
            (set! (fimg (+ offset 2)) (/ (fimg (+ offset 2)) nsub*nsub))
            (set! (image (+ offset 0)) (byte (clamp (fimg (+ offset 0)))))
            (set! (image (+ offset 1)) (byte (clamp (fimg (+ offset 1)))))
            (set! (image (+ offset 2)) (byte (clamp (fimg (+ offset 2)))))))))
    image)

  )

(def ^:const WIDTH 256)
(def ^:const HEIGHT 256)
(def ^:const NSUBSAMPLES 2)

(defn save-ppm [width height ^bytes image ^String filename]
  (with-open [fos (FileOutputStream. filename)]
    (.write fos (.getBytes (str "P6\n" width \space height "\n255\n")))
    (.write fos image)))

(defn -main [& args]
  (let [bench (.. (AOBench. WIDTH HEIGHT NSUBSAMPLES)
                  (spheres (into-array [(Sphere. (Vec. -2.0 0.0 -3.5) 0.5)
                                        (Sphere. (Vec. -0.5 0.0 -3.0) 0.5)
                                        (Sphere. (Vec.  1.0 0.0 -2.2) 0.5)]))
                  (plane (Plane. (Vec. 0.0 -0.5 0.0) (Vec. 0.0 1.0 0.0))))
        image (.render bench)]
    (save-ppm WIDTH HEIGHT image "ao.ppm")))
