Bass Diffusion Model*
===

"The model describes a product diffusion process. Potential adopters
of a product are inflenced into buying the product by advertising
and by word of mouth from adopters -- those who have already
purchased the new product. Adoption of a new product driven by
word of mouth is likewise an epidemic. Potential adopters come into
contact with adopters through social interactions. A fraction of these
contacts results in the purchase of the new product. The advertising
causes a constant fraction of the potential adopter population to
adopt each time period."

The source of this implementation is
[David Sorokin's work](http://hackage.haskell.org/package/aivika-0.1).
ou may find David Sorokin's original work at <https://github.com/dsorokin/aivika>

Commented it to better see the flow of implementation and the
approach of ABM taken.

If you want to read the comments, begin with the `main` function.
To try the simulation simply `:load` this file in ghci and run `main`.

> import Random
> import Data.Array
> import Control.Monad
> import Control.Monad.Trans

> import Simulation.Aivika.Dynamics

The number of agents `n`:

> n = 500

> advertisingEffectiveness = 0.011
> contactRate = 100.0
> adoptionFraction = 0.015

> specs = Specs { spcStartTime = 0.0,

Eight time points:

>                 spcStopTime = 8.0,

Ten steps between each two time points:

>                 spcDT = 0.1,

Applying a 4th order Runge-Kutta method:

>                 spcMethod = RungeKutta4 }

One of two Random Number Generators:

> exprnd :: Double -> IO Double
> exprnd lambda =
>   do x <- getStdRandom random
>      return (- log x / lambda)

Second of two Random Number Generators:

> boolrnd :: Double -> IO Bool
> boolrnd p =
>   do x <- getStdRandom random
>      return (x <= p)

Each person is identified with an agent.
Each agent can have two states, potential or adopter alone:

> data Person = Person { personAgent :: Agent,
>                        personPotentialAdopter :: AgentState,
>                        personAdopter :: AgentState }

> createPerson :: DynamicsQueue -> Dynamics Person              
> createPerson q =    
>   do agent <- newAgent q
>      potentialAdopter <- newState agent
>      adopter <- newState agent
>      return Person { personAgent = agent,
>                      personPotentialAdopter = potentialAdopter,
>                      personAdopter = adopter }

Given event queue, returning array:

> createPersons :: DynamicsQueue -> Dynamics (Array Int Person)
> createPersons q =

Create 500 (`n = 500`, given above) persons (next see `createPerson`):

>   do list <- forM [1 .. n] $ \i ->
>        do p <- createPerson q
>           return (i, p)
>      return $ array (1, n) list

> definePerson :: Person -> Array Int Person 
>                -> DynamicsRef Int -> DynamicsRef Int
>                -> Dynamics ()
> definePerson p ps potentialAdopters adopters =
>   do stateActivation (personPotentialAdopter p) $

... `personPotentialAdopter` state of the agent `p` can be activated with the help of this...
How? Increase number of agents having `personPotentialAdopter` state

>        do modifyRef' potentialAdopters $ \a -> a + 1

In this monadic computation add a timeout, "making the agent alive"
first compute a time period in which the added timeout can be actuated

>           t <- liftIO $ exprnd advertisingEffectiveness 
>           let st  = personPotentialAdopter p
>               st' = personAdopter p

The handler (timeout) is given (as arguments) the state (`personPotentialAdopter`) it is assigned
to, the time period in which it can be actuated - if `personPotentialAdopter` (the state)
will remain active and the third argument defines the corresponded computation
(which literally is "activate the `personAdopter` state")

If this handler is still actuated it happens only once as opposed to timer,
... does making sense as the transition to adopter is a one-step.

>           addTimeout st t $ activateState st'
>      stateActivation (personAdopter p) $ 

... `personAdopter` state of the agent `p` can be activated with the help of this...
How? Increase number of agents having `personAdopter` state, in case of success
(is what means "monadic" in this case of a computation) of the overall subsequent computation,
by updating the event queue ref.

>        do modifyRef' adopters  $ \a -> a + 1

In this monadic computation add a timer that works while the state is active,
"making the agent alive", the timer is assigned to the `personAdopter` state,
can be actuated within time period `t` and has the corresponded compuation defined
in the nested `do`... (next see *+)

Will periodically repeat while the (`personAdopter`) state remains active.

>           let t = liftIO $ exprnd contactRate    -- many times!
>           addTimerD (personAdopter p) t $

`i` is the number of any one of 500 (amount of agents `n`, defined above) persons:

>             do i <- liftIO $ getStdRandom $ randomR (1, n)

*+ the compuation corresponded to the timer handler.
Take the `i`th person number from the array:

>                let p' = ps ! i

Determine the agent's (belonging to the drawn person number) "downmost active" state (as `st`):

>                st <- agentState (personAgent p')

When the "downmost active" state is `personPotentialAdopter`...

>                when (st == Just (personPotentialAdopter p')) $

... determine if/or not this agent should belong the `adopters`
this is a bool (named `b`) now, so there is a fifty-fifty chance...
... in other words an agent being currently a potential adopter has a
fifty-fifty chance now to belong to the `adopters` (or adopters fraction of the
500 consumers) by state activation in a moment... AARGH yet too complicated to even spell

>                  do b <- liftIO $ boolrnd adoptionFraction

event queue won't be updated if not...

>                     when b $ activateState (personAdopter p')
>      stateDeactivation (personPotentialAdopter p) $

... `personPotentialAdopter` state of the agent `p` can be deactivated with the help of this...
How? Simply decrease number of agents having `personPotentialAdopter` state,
updating the event queue ref. Effect: All handlers (timer, timeout) are outdated and will be ignored
but may be assigned new ones at time of next state activation

>       modifyRef' potentialAdopters $ \a -> a - 1
>      stateDeactivation (personAdopter p) $

... `personAdopter` state of the agent `p` can be deactivated with the help of this...
How? Simply decrease number of agents having `personAdopter` state, updating the event queue
ref. Effect: All handlers (timer, timeout) are outdated and will be ignored
but may be assigned new ones at time of next state activation

>       modifyRef' adopters $ \a -> a - 1

Given the persons array and the event queue refs to potential and real adopters,
prepare the `Dynamics` computation, next see `definePerson`, where the real `Dynamics` computation,
composed of activation and deactivation computations as well as timer and timeout handlers
is defined:

> definePersons :: Array Int Person 
>                 -> DynamicsRef Int 
>                 -> DynamicsRef Int 
>                 -> Dynamics ()
> definePersons ps potentialAdopters adopters =
>   forM_ (elems ps) $ \p -> 
>   definePerson p ps potentialAdopters adopters

"Initiating agent and selecting another downmost active state" (state machine
approach to agents, see the ABM explanation in David Sorokin's paper - link at top of this page),
because its a product diffusion model, activating the
`personPotentialAdopter` state of the person always first:

> activatePerson :: Person -> Dynamics ()
> activatePerson p = activateState (personPotentialAdopter p)

Run through the array, activating agents:

> activatePersons :: Array Int Person -> Dynamics ()
> activatePersons ps =
>   forM_ (elems ps) $ \p -> activatePerson p

> model :: Dynamics (Dynamics [Int])
> model =
>   do q <- newQueue

An agent (representing consumers) can be in two states,
   potential adopter or

>      potentialAdopters <- newRef q 0 -- empty event queue

   adopter

>      adopters <- newRef q 0          -- empty event queue

To create the person, we need the event queue (`q`).
We place all persons in the array (`ps`, next see `createPersons`)
We need this array to have an access to random agents at time when the
specified adopter tries to convert somebody to be an adopter too

>      ps <- createPersons q

Create the agents/objects and define their activation and deactivation
computations (next see `definePersons`)

>      definePersons ps potentialAdopters adopters

Activate the agents in the array each, remember the lazy approach, this is
preparation all, executed only when the event queue reference is read

>      activatePersons ps

Return pair (per simulation step?!!) expressing amount of potential
adopters remaining and adopters already existent, reading each entry from the
particular event queue reference (`potentialAdopters`, `adopters`)

>      return $ do i1 <- readRef potentialAdopters
>                  i2 <- readRef adopters
>                  return [i1, i2]

> main =

Start here (specs defined above, next see `model`).
Running the model means - "creating a simulation and then running it
in all integration time points using the specified simulation steps"
- in aivikas terms

(`runDynamics :: Dynamics (Dynamics a) -> Specs -> IO [a]`)

>   do xs <- runDynamics model specs

For the above concrete specification (80 steps in 8 time points) yielding 81 pairs,
the initial pair included.

>      print xs

