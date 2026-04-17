
import qualified Control.Monad.RWS as RWS

myComp :: RWS.RWS () () Int Int 
myComp = RWS.rws $ \() s  -> (s*2 , s , ())

counter :: RWS.RWS () [String] Int Int 
counter = RWS.RWST $ \() s -> return (s, s-1, ["tick " ++ show s])

rwsExample :: RWS.RWS Int [String] Int Int 
rwsExample = 
    RWS.ask >>= \coeff -> 
    RWS.get >>= \counter -> 
        RWS.put (counter + 1) >>
        RWS.tell ["inc counter to " ++ show (counter +1)] >>
        return (coeff * counter )