-- Reader Monad (MReaderpr.hs): 
--     ask — Get the whole environment
--         ask :: Reader r r
--         ask = reader id

--     asks — Get a specific piece of the environment
--         asks :: (r -> a) -> Reader r a
--         asks f = reader f   (essentially the same as reader!)

--     local — Temporarily modify the environment
--         local :: (r -> r) -> Reader r a -> Reader r a


-- Writer Monad 
    
--     tell — Writing to the Log
--         tell :: Monoid w => w -> Writer w ()

--     listen — Peeking at the Intermediate Log
--         listen :: Monoid w => Writer w a -> Writer w (a, w)
--         **remember how listen and listens change the type of the writer** 
--     listens — Transform the Peeked Log
--         listens :: Monoid w => (w -> b) -> Writer w a -> Writer w (a, b)
--         listens f action
--             same as: fmap (\(a, w) -> (a, f w)) (listen action)
    
--     censor — Modifying the Log
--         censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a

-- State Monad 

--     Interface: get, put, modify, gets

--         get — read the current state
--             get :: State s s
--             get = state $ \s -> (s, s)
--             runState get 42   -- → (42, 42)
        
--         put — overwrite the state
--             put :: s -> State s ()
--             put s = state $ \_ -> ((), s)
--             runState (put 99) 0   -- → ((), 99)

--         modify — apply a function to the state
--             modify :: (s -> s) -> State s ()
--             modify  f = do
--                     s <- get        -- read current state
--                     put (f s)       -- write f(state) as new state
--             runState (modify (+10)) 5   -- → ((), 15)

--         gets — read the state, transform it, use as result  
--             gets :: (s -> a) -> State s a
--             gets    f = do
--                     s <- get
--                     return (f s)
--             runState (gets (*2)) 5    -- → (10, 5)


                -- Function	Returns	Use when you want
                -- runStateT	m (a, s)	Both the result and the final state
                -- evalStateT	m a	Only the result, discard the state
                -- execStateT	m s	Only the final state, discard the result
