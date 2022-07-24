
(let
  ( (newtype #List (a)
      ( (Cons (rec (head a) (tail (#List a))))
        (Nil  #Unit)
      )
    )

    (nil  (Nil ()))
    (cons (fun (x xs) (Cons (rec (head x) (tail xs)))))

    (+    (ffi (fun (#Int #Int) #Int)))
  )
(let
  ( (pure (fun (x) (cons x nil)))
    (fold
      (fun (op zero list)
        (case list
          ((Cons (rec head tail)) (op head (fold op zero tail)))
          ((Nil  u)               zero))
      )
    )
  )
(let ( (append (fun (xs ys) (fold cons ys xs))) )
(let ( (join   (fun (lists) (fold append nil lists))) )
(let ( (two (fold + 0 (join (pure (pure 2))))) )
  two
)))))
