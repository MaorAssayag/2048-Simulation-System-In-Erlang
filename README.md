<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/2048_logo.svg/220px-2048_logo.svg.png" width="130">

# 2048-Simulation-System-In-Erlang
Concurrent and Distributed 2048 Simulation Systems implemented in Erlang OTP (Ver 21.0).

</br> </br> 
## Background
2048 is a simple and addictive network game, with simple rules: the board includes 16 squares, with colored squares with double numbers. The controls are 4 arrow keys on the elements that move the squares as far as possible to the right, left, up or down. After each slide a new square appears on the screen.

When the player pushes two adjacent squares with the same value, for example 8 and 8, they connect and turn into the sum of 16 numbers in this case. The goal is to reach a square of 2048. If all the tiles in the board are occupied and there is no more space to move, you are disqualified and a new game must begin. A round or two is enough to understand the principle, and you can see a live demo of the game by clicking <a href="http://2048game.com/">here</a>.

<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/git_asset1.jpg" width="300">

</br> </br> 
## The Basic Idea
The system is divided into 2: a decentralized and integrated **simulation center** and a live **user game** of 2048 with a real-time response.

The **simulations center** allows the running of simulations of games in the background according to the definition of running characteristics in the user interface (choose algorithm for a bot, desired slot value for the win and the number of desired runs) and receiving statistics from the total number of servers connected to the main server.  
The system has protection against falls, so if a simulation server falls and does not suffice to provide statistics on all the current runs it was asked to perform, its remaining task will also be distributed to the other simulation servers.

In addition to maintained **backup server** that allows the system's ability to automatically recover from the *main server* crash and selecting one of the simulation servers to function as a primary server.

During the simulations the information is frequently backed up to the backup server so that a new main server is created, it does not lose the information we have collected so far.

The real-time game is maintained by two servers (the **user interface server** and the **main server** with a dedicated game management process), where each cube is a process that holds value and location.  
The user game system is built with a process pyramid mechanism (to be expanded later) to collect and maintain information on the board.

<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/1.PNG" width="600">

</br> </br> 
## System structure
The system was developed in Erlang (21.0), spread over 2,500 lines of code and divided into 3 main entities:

**1. Main computer (single at a time)**  
  Where the main server and the ui_server server will run.  
  - Management of the graphical display and user interface - real-time game, simulation console and statistics in real time.  
  - Responsibility for communication and data transfer between the simulation servers (secondary) and the user interface as well as backup on the backup server.  
  - Responsibility for managing the processes, logic and logic of the game in real time.  
  - Monitoring the secondary servers and the backup server to protect against falls for proper operation.    

**2. A secondary computer (multiple)**  
  On which a simulation server will run, which connects to the main server that monitors it (connect).  
  - Continually receiving requests for the desired number of simulations. For each game, a process is started that simulates bot according to the simulation parameters.  
  - Updating the simulation parameters that decide how the bot will choose the next step - from naive algorithms (random moves, maximum scoring) to algorithms used in machine learning (see below).  
  - Send an asynchronous response to the main server with intermediate results on the simulator statistics to update the graphic display.  
  - If necessary, a secondary computer will kill the simulation server and become the primary server while preserving the collected data.  

**3. A backup computer (single)**    
  On which the backup server will be run, to which the main server connects.
  - Maintain an ets-based database for all simulation data (for each parameter combination, the statistics collected so far must be kept).  
  - Receive frequent updates from the main server and update the database.  
  - Main server monitoring and crash protection - if the main server backup machine initiates the request to establish a new main server from one of the simulation servers.  
  - If a primary server is reloaded, the data recovery from the backup must be performed automatically for the user (as well as in the user interface).  
  
 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/4.png" width="600">
 
</br> </br> 
## Main Server
**Properties**  
  - The main server (main_server) that communicates directly with the user interface (ui_server).    
  - Graphics display applications and user interface - real-time game, simulation console and statistics in real time.    
  - Responsibility for communication and data transfer between the simulation servers (secondary) and the user interface as well as backup on the backup server.    
  - Responsibility for managing the processes, logic and logic of the game in real time.  
  - Monitoring the secondary servers and the backup server to protect against falls for proper operation.  
  - Realized using gen_server  
  - If a simulation server falls and provides only partial information (ie, missing statistics on unapproved simulations), then the main server will replace the remaining tasks and add to the remaining secondary servers in real time.  
  - Maintaining the user's game mode - including calculating the new game mode, whether the game is over, whether the user has lost or won (analyzing the data structures in which the new game is stored).  
  - The game that the user is performing in Live, we have decided to accept the inter-process communication.  
  
**Architecture uses gameplay**  
  - A master process that manages the game and communication against all sub-processes, works concurrently with the main_server server and receives requests from it directly with the user's move. Role of the master process to provide the main server the new board after the move, the game mode (finished \ win or loss) and current score.  
  - The main server has a data structure that tracks the sub-processes that make up the board, is the only one that will create or destroy them.  
  - 4 top processes that are responsible for communicating with a maximum of 4 slave processes each.
  Each slave process holds a value and location, and can modify it or send it to the pid that is attached to the application.  
  - In making a new move, the master server will ask 4 top servers to supply the 4 lines of the board. Each top process will receive a list of the processes on its line with the request, and it must return a reply to the master after adding its line values.  
  - At the end of the board collection, the master will activate the game logic with the desired move on the current board, run analysis to check the end of the game \ victory and score and send the results with the new board to the main server.  
  - The main server will create new processes / kill processes that are no longer listed and update the process board on record in state.  

 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/5.png" width="600">
