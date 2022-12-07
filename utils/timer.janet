(defmacro with-timer
  [& body]
  (def $s1 (gensym))
  (def $e1 (gensym))
  (def $out (gensym))
  ~(let [,$s1 (os/clock)
         ,$out (do ,;body)
         ,$e1 (os/clock)]
     (printf "took %fms" (* (- ,$e1 ,$s1) 1000))
     ,$out))
