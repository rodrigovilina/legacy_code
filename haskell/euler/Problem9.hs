main = print (
  product (
    head (
      take 1 [[a,b,c] |
        a <- [1..1000],
        b <- [1..1000],
        c <- [1..1000],
        a+b+c == 1000,
        a^2 + b^2 == c^2
        ]
      )
    )
  )
