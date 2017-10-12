# GridWorld

This is an assignment of COMP9414(Artificial Intelligence) of UNSW.

Language: prolog.

The Gridworld consists of a two-dimensional grid of locations, extending to infinity in both directions. Truffles and restaurants appear at certain locations. The agent is able to move to a place where truffles are located and execute a pick action. After collecting a sufficient number of truffles, it can move to the location of a restaurant and execute a sell action. The agent can stay where it is, or move one square at a time, either horizontally or vertically. The world is dynamic in that truffles and restaurants can appear spontaneously at random locations at any time.

The supplied Prolog program gridworld.pl implements a system for conducting an experimental trial consisting of an agent in the Gridworld that repeatedly executes the BDI interpretation cycle for 20 iterations (this is a deliberately small number for ease of writing and debugging the program). The initial state of the world is always that there are no truffles or restaurants, and the agent is at location (5,5) holding no truffles.

The agent's beliefs at any time are in the form beliefs(at(X,Y),stock(T)) meaning that the agent is at location (X,Y) and is currently holding a stock of T truffles. The initial belief state of the agent is represented by beliefs(at(5,5),stock(0)).

The agent's goals at any time are in the form goals(Goals_rest,Goals_truff) where Goals_rest is a list of locations of restaurants and the number of truffles they wish to buy, and Goals_truff is a list of locations of truffles, and the number of truffles at that location. Each goal of the agent is represented as a term goal(X,Y,S), where (X,Y) is the location of either a truffle or a restaurant, and S is the number of truffles.

The agent's intentions are in the form intents(Intents_sell,Intents_pick) where Intents_sell and Intents_pick each consist of a list of pairs of the form [Goal, Plan], representing a goal with an associated plan (which may be the empty plan), ordered according to some priority.

Each plan is a list of actions. To fulfil an intention, the agent executes the plan associated with its goal, which will make the agent move along a path towards the goal and then either pick or sell truffles. If, when the agent chooses an intention to fulfil, the plan associated with the goal of that intention is empty or cannot be executed, the agent creates a new plan for the goal and then begins to execute this plan.

In each cycle the agent executes one action. There are three types of action the agent can execute:

    move(X,Y) - the agent moves to location (X,Y)
    pick(X,Y) - the agent picks up the truffles at (X,Y)
    sell(X,Y) - the agent sells truffles to the restaurant at (X,Y) and scores the associated points
    




Destailed discription:
http://www.cse.unsw.edu.au/~cs9414/17s1/hw3prolog/
