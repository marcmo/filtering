
ioactions = [print "hey", putStr "ho"]

main = do head ioactions
          last ioactions

myliftM :: (a -> b) -> (IO a -> IO b)
myliftM f action = do x <- action
                      return (f x)

twice x = 2*x

