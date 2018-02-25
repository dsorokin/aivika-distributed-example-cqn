
# A Closed Queue Network Model for Distributed Aivika

This is a closed queue network model described in article "Performance evaluation of conservative algorithms in parallel simulation languages" (2000) by R. L. Bagrodia and M. Takai. Also this model is described in article ["Parallel simulation made easy with OMNeT++"](https://www.semanticscholar.org/paper/Parallel-Simulation-Made-Easy-With-OMNeT%2B%2B-Varga/fe5a96d4ca8125e407214d5195c45a65fc543a6d?tab=abstract) (2003) by Varga, A., Sekercioglu, A.Y.

The model consists of N queue tandems where each tandem consists of a switch and k single-server queues with exponential service times. The last queues are looped back to their switches. Each switch randomly chooses the first queue of one of the tandems as destination, using uniform distribution. The queues and switches are connected with links that have nonzero propagation delays. 

In the mentioned articles the model was used for estimating the conservative distributed simulation methods and their implementations. Here the same model can be used for estimating the Aivika implementation of the optimistic Time Warp method.

### Prerequisites

To repeat the test, you need a simulation cluster. It can be either a single computer such as laptop or a true cluster consisting of different computers.

The code is written in Haskell. In the simplest case you need [Stack](http://docs.haskellstack.org/) installed on your nodes of the cluster. To reproduce the test, you don't need to know the Haskell programming language, though.

### Downloading from GitHub

Download the test code from GitHub on all your nodes:

```
$ git clone https://github.com/dsorokin/aivika-distributed-example-cqn.git
$ cd aivika-distributed-example-cqn
```

### Defining Cluster Topology

Now you have to define a topology of the cluster. In other words, you have to decide, where the logical processes will reside in and which ports they will listen to. By the way, the ports must be open. The most simple way is to create a local network.

You have to edit the configuration file on every node of the cluster. The simplest way is to edit the default configuration file `cqn.conf` located in the root directory. I will use this file below, but you can specify other configuration files as well.

For example, I decided to use two laptops connected via the Ethernet cable in local network. The first laptop works under macOS and has IP address 192.168.99.20. The second laptop works under Linux and has IP address 192.168.99.10. But if you decide to repeat the simulation experiment on one physical computer, then you can define all IP addresses as `127.0.0.1` . It will work too.

So, if I want to simulate 4 queue tandems then I have to define four sections `lp1`, `lp2`, `lp3` and `lp4`. These sections will describe the residence of 4 logical processes, each of which will simulate the corresponding queue tandem.

In my case the `cqn.conf` file will have the following contents:

```
# Logical process number 1 in case of distributed simulation.
[lp1]
host = 192.168.99.20
port = 8081

# Logical process number 2 in case of distributed simulation.
[lp2]
host = 192.168.99.20
port = 8082

# Logical process number 3 in case of distributed simulation.
[lp3]
host = 192.168.99.10
port = 8083

# Logical process number 4 in case of distributed simulation.
[lp4]
host = 192.168.99.10
port = 8084
```

It means that the first two logical processes will reside in host 192.168.99.20, while the last two logical processes will reside in other host 192.168.99.10. I could use 127.0.0.1 instead of these IP addresses. Then all logical processes would reside in my single computer.

### Building the Project

For the first time, you will have to set up the Stack project and then compile it to build a binary executable.

```
$ stack setup
$ stack build
```

### Running Auxiliary Simulation Nodes

Now the time is to run the auxiliary simulation nodes. In my case one auxiliary node is located on the laptop with IP address 192.168.99.20. It will correspond to the `lp2` section.

So, I type in the Terminal window:

`$ stack exec aivika-distributed-example-cqn cqn.conf slave 2`

Now I have to switch to another laptop with IP address 192.168.99.10, which works under Linux if you remember. I launch two computational node instances one by one, starting from the third logical process

`$ stack exec aivika-distributed-example-cqn cqn.conf slave 3`

and then proceeding with the fourth logical process

`$ stack exec aivika-distributed-example-cqn cqn.conf slave 4`

### Launching Distributed Simulation

Now I will run the distributed simulation on my cluster. I switch to the first laptop with IP address 192.168.99.20 and type in another Terminal window:

`$ stack exec aivika-distributed-example-cqn cqn.conf master 1`

The simulation will start and finish soon. I will see something like this: 

```
Started simulating in the distributed mode...
Master's result: 522
[ERROR] Exception occurred: ProcessTerminationException
[ERROR] Exception occurred: ProcessTerminationException
```

The last two messages are very helpful as they indicate that the corresponding processes had finished. The resulting value is a number of dequeue counts from the last queue in the corresponding tandem. Since the queue network is closed, the same transacts pass the tandems again and again. This value allows estimating the complexity of operations made within simulation. Also we can see that everything is OK.

Congratulations! You have started the Aivika distributed simulation and received the result!

### Estimating Speed of Simulation

Actually, this model can be used for estimated the distributed module of Aivika. If you look at the `cqn.conf` configuration file, then you will see the `simulation-mode` parameter:

```
# The simulation mode. It can be either "sequential", "pseudo-sequential", "parallel" or "distributed".
simulation-mode = distributed
```

We used only one possible mode. But in case of other simulation modes, there is no need to launch auxiliary nodes. Also the configuration file is a single required argument for these modes.

So, if you change this parameter then the simulation test can be launched by the following command:

`$ stack exec aivika-distributed-example-cqn cqn.conf`

In general, the meaning of possible options are as follows:

- The `sequential` mode uses the sequential library, which is very and very fast.

- The `pseudo-sequential` mode means that there is only one logical process, where the model is equivalent to the sequential model but only the simulation is launched with help of the distributed module.

- The `parallel` mode means that there are the corresponding number of logical processes that reside in the same computational node of the same host. Each logical process represents a separate queue tandem.

- The `distributed` mode is similar to the parallel mode, but only every logical process resides in a separate computational node, which we have to configure manually them in sections [lp1], [lp2], ... . It allows creating true distributed simulations using different computers.

There are other parameters as well, where two of the most important parameters define the final time point and the number of queue tandems:

```
# The final simulation time.
stop-time = 10000

# The number of tandem queues.
tandem-queues = 4
```

This is yet a subject of the further investigation to know how fast is the distributed module of Aivika and where it can give us real benefits.
