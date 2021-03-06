module Main where

import Control.Monad
import Pipes
import Pipes.Core 
import Pipes.Internal 

-----------------------------------------------------------------------------

main :: IO ()
main = do
  runEffect $ producer >-> pipe >-> consumer
  runEffect $ producer' >-> pipe' >-> consumer'
  runEffect $ producer'' >-> pipe' >-> consumer'
  runEffect effect >>= print
  runEffect effect' >>= print
  runEffect effect''
  runEffect effect'''

-----------------------------------------------------------------------------

producer :: Producer Int IO ()
producer = each [1,2]

pipe :: Pipe Int Int IO ()
pipe = forever $ await >>= yield . (+3)

consumer :: Consumer Int IO ()
consumer = forever $ await >>= lift . print

-----------------------------------------------------------------------------

producer' :: Proxy X () () Int IO ()
producer' = Respond 1 (\() -> Respond 2 (\() -> Pure ()))

pipe' :: Proxy () Int () Int IO ()
pipe' = Request () (\v -> Respond (v+3) (\() -> pipe))

consumer' :: Proxy () Int () X IO ()
consumer' = Request () (\v -> M (print v >> return consumer))

-----------------------------------------------------------------------------

producer'' :: Proxy X () () Int IO ()
producer'' 

  -- = yield 1 >> yield 2
  
  -- = yield 1 >>= \_ -> yield 2
  
  -- = Respond 1 Pure >>= \_ ->  Respond 2 Pure
  
  -- = Respond 1 (\v -> Pure v >>= \_ -> Respond 2 Pure)
  
  = Respond 1 (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v'))

-----------------------------------------------------------------------------

effect :: Effect IO String
effect

  -- = lift (print "abc" >> return "xyz")

  = M ((print "abc" >> return "xyz") >>= \r -> return (Pure r))

-----------------------------------------------------------------------------

effect' :: Effect IO ()
effect'

  = (Respond (1::Int) (\() -> Pure ())) >-> (Request () (\_ -> Pure ()))

  -- = (\() -> Respond 1 (\() -> Pure ())) +>> Request () (\() -> Pure ())

  -- = Respond 1 (\() -> Pure ())) >>~ (\() -> Pure ())

  -- = (\() -> Pure ()) +>> Pure ()

  -- = Pure ()

-----------------------------------------------------------------------------

effect'' :: Effect IO ()
effect''

  -- = producer'' >-> pipe' >-> consumer'

  = Respond 1 (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v'))
      >-> (Request () (\v -> Respond (v+3) (\() -> pipe'))
          >-> Request () (\v -> M (print v >> return consumer')))

  -- Expand (>->) to (+>>)

  -- = (\() -> Respond 1 (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v')))
  --   +>> (\() -> Request () (\v -> Respond (v+3) (\() -> pipe')))
  --       +>> (Request () (\v -> M (print v >> return consumer'))) 
  
  -- Apply consumer' request to pipe'

  -- = (\() -> Respond 1 (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v')))
  --   +>> (Request () (\v -> Respond (v+3) (\() -> pipe')))
  --       >>~ (\v -> M (print v >> return consumer'))

  -- Apply pipe' request to producer

  -- = (Respond 1 (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v')))
  --   >>~ (\v -> Respond (v+3) (\() -> pipe'))
  --       >>~ (\v -> M (print v >> return consumer'))

  -- Apply producer response to pipe'

  -- = (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v'))
  --   +>> (Respond 4 (\() -> pipe'))
  --       >>~ (\v -> M (print v >> return consumer'))

  -- Apply pipe' response to consumer'

  -- = (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v'))
  --   +>> (\() -> pipe')
  --       +>> (M (print 4 >> return consumer'))

  -- Add pipe' to contination returned by base monad print action

  -- = (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v'))
  --       +>> (M ((print 4 >> return consumer') 
  --           >>= (\p' -> return ((\() -> pipe') +>> p'))))        

  -- Add producer to contination returned by base monad print action

  -- = M ((print 4 >> return consumer') 
  --     >>= (\p' -> return ((\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v')) 
  --         +>> (\() -> pipe') +>> p')))

-----------------------------------------------------------------------------

effect''' :: Effect IO ()
effect'''

  = (\v -> Pure v >>= \_ -> Respond 2 (\v' -> Pure v')) 
      +>> (\() -> pipe')
          +>> consumer'     
  
