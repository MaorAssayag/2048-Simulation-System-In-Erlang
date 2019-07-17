<img src="https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/2048_logo.svg/220px-2048_logo.svg.png" width="130">

# 2048-Simulation-System-In-Erlang
Concurrent and Distributed 2048 Simulation Systems implemented in Erlang OTP (Ver 21.0).
</br> </br> 
## Background
2048 is a simple and addictive game, with simple rules: the board includes 16 squares, with colored squares with double numbers. The controls are 4 arrow keys on the elements that move the squares as far as possible to the right, left, up or down. After each slide a new square appears on the screen.

When the player pushes two adjacent squares with the same value, for example 8 and 8, they connect and turn into their sum, 16. The goal is to reach a square of 2048. If all the tiles in the board are occupied and there is no more space to move, you are lost and a new game must begin. A round or two is enough to understand the principle, and you can try a live demo of the game by clicking <a href="http://2048game.com/">here</a>.

<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/git_asset1.jpg" width="300">

</br> </br> 
## The Basic Idea
The system is divided into 2: a decentralized and integrated **simulation center** and a live **user game** of 2048 with a real-time response.

The **simulations center** allows the running of simulations of games in the background according to the simulation parameters in the user interface (choose algorithm for a bot, desired tile value for a win and the number of desired simulations) and receiving statistics from the total number of servers connected to the main server.  
The system has protection against falls, so if a simulation server falls and does not suffice to finished its games, its remaining task will also be distributed to the other simulation servers.

In addition to maintained **backup server** that allows the system's ability to automatically recover from the *main server* crash and selecting one of the simulation servers to function as a main server.

During the simulations the information is frequently backed up to the backup server so that a new main server is created, it does not lose the information we have collected so far.

The real-time game is maintained by two servers (the **user interface server** and the **main server** with a dedicated game management process), where each cube is a process that holds value and position.  
The user game system is built with a process pyramid mechanism (to be expanded later) to collect and maintain information on the board.

<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/1.PNG" width="600">

</br> </br> 
## System structure
The system was developed in Erlang (21.0), spread over 2,500 lines of code and divided into 3 main entities:

**1. Main computer (single at a time)**  
  Where the main server and the ui_server server will run.  
  - Management of the graphical and user interface - real-time game, simulation console and statistics in real time.  
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
  - Implemented using gen_server  
  - If a simulation server falls and provides only partial information (ie, missing statistics on unapproved simulations), then the main server will replace the remaining tasks and add to the remaining secondary servers in real time.  
  - Maintaining the user's game mode - including calculating the new game mode, whether the game is over, whether the user has lost or won (analyzing the data structures in which the new game is stored).  
  - The game that the user is performing in Live, we have decided to accept the inter-process communication.  
  
**Architecture useed in the live gameplay**  
  - A master process that manages the game and communication against all sub-processes, works concurrently with the main_server server and receives requests from it directly with the user's move. Role of the master process to provide the main server the new board after the move, the game mode (finished \ win or loss) and current score.  
  - The main server has a data structure that paths the sub-processes that make up the board, is the only one that will create or destroy them.  
  - 4 top processes that are responsible for communicating with a maximum of 4 slave processes each.
  Each slave process holds a value and location, and can modify it or send it to the pid that is attached to the application.  
  - In making a new move, the master server will ask 4 top servers to supply the 4 lines of the board. Each top process will receive a list of the processes on its line with the request, and it must return a reply to the master after adding its line values.  
  - At the end of the board collection, the master will activate the game logic with the desired move on the current board, run analysis to check the end of the game \ victory and score and send the results with the new board to the main server.  
  - The main server will create new processes / kill processes that are no longer listed and update the process board on record in state.  

 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/5.png" width="800">

</br> </br> 
## UI Server
**Properties**  
  - Implemented as **wx_object**  
  - The server is the user interface (ui_server) that communicates directly with the main server (main_server).  
  - Management of the graphical display and user interface - real-time game, simulation console and statistics in real time.  
  - Identifying user-pressed keys (arrow keys and spaces), communicating with the main server to request a response  
  - Manage graphical elements in the user interface (server tables and their status, statistics, setting simulation parameters, user game and more)  
  
**UI**
- **Left Zone** - A real-time game controlled by the user. The game is maintained on the main server by a process logger (each slot is a process as part of a "pyramid" of processes that interact with them).  
- **Right Zone** - Game Simulation Console.
Allows the user complete control over the creation of x simulations (bots) on simulation servers that connect to the main server (other nodes.) You can control the simulation parameters (win threshold for bot, the next step selection method for the bot)  
- **Lower Zone** - Status Statistics that are updated frequently from the simulation servers through the main server.  

 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/simulations1.PNG" width="600">  
 
 </br> </br> 
## Backup Server
**Properties**  
  - Implemented using **gen_server**
  - Its function is to back up all the data collected so far in simulations (for each simulation parameter combination), communicates directly with the main server (main_server) and, if necessary, with simulation servers (simulation_server).
  - Monitoring the central server to protect against falls to continue normal operation - If the main server is down, the backup server will select one of the simulation servers and ask it to become a primary server. Switching to a new UI display (after the main server has been dropped) is immediate.
  - When a new primary server is installed, the backup server will send an update message with the current database rule to continue normal operation. The new main server also monitors the backup server so that we can update the user interface according to the status of the backup server.  
  
 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/backup.png" width="500">  

 
  </br> </br> 
## Simulation Server
**Properties**  
  - Implemented using **gen_server**
  - Multiple, not limited to the number of stations (Nodes) that can establish it and connect to the main server.
  - Each simulation server will receive the number of simulations it is required to perform (for example, 200 simulations of games are required so it will set up 200 bot processes when each process is required for a game icon).
  - Each such process will calculate the game locally and its mode according to the actions (bot) it manages.
  - Initializes a table of ets that is common to all processes on the server - To prevent a bottleneck of update messages from the various processes, you can directly update the results (by atomic operations in the ets directory).
  - When a primary server is down, the backup server selects a simulation server and asks it to become a primary server instead (ie close the simulation server and set up a main server).
  - The simulation parameters determine how each bot will manage the game, a detailed explanation below.


  </br> </br> 
## Simulation parameters
**Threshold**  
Threshold value allows bots to win the game. Without learning a machine or taking into account in-depth calculation (for each step to calculate a few steps ahead), reaching the value of 2048 in a slot to win is a complex task. To do this, we can define the threshold value of the threshold and thus see slightly more interesting data. We allow the threshold value to be changed to [2048, 1024, 512, 256, 128].  

**Decision ID**  
In order to enable variation in simulations, we can choose between 5 modes of action for the bots behavior when they are required to perform the following step:  

- **Random**  
The bot performs random moves that are evenly distributed
[up, down, right, left]. This simulation is naturally the fastest and the number of wins is expected to be the smallest of a large sample and compared to other methods.

- **Max score**   
  The bot calculates the 4 possible results after each move separately [up, down, right, left].  
  For results we filter for games that are not over (ie we have not lost). From these games we choose the game with the maximum score and proceed to the next step. Of course if all moves lead to a dead end (the game is over) then the bot will summarize the results.  

  For each separate calculation, not only does the logic of the game follow the desired move (eg sliding to the right), but also analyzes the state of the game (ended / victory).  
 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/maxscore.png" width="450">  


- **Stay Alive**  
  The bot calculates sequentially, if the move leads to immediate loss, try to calculate a different displacement. When the goal is to make as many moves as possible (stay alive).  

  For each separate calculation, not only does the logic of the game follow the desired move (eg sliding to the right), but also analyzes the state of the game (ended / victory).
 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/stayalive.png" width="400">  



- **Max Merged Tiles**   
  The bot calculates the 4 possible outcomes after performing each move separately [up, down, right, left]. For results we filter for games that are not over (i.e. we have not lost).  
  From these games we select the game with the maximum number of squares and proceed to the next step. Of course if all moves lead to a dead end (the game is over) then the bot will summarize the results.  

  For each separate calculation, not only does the logic of the game follow the desired move (eg sliding to the right), but also analyzes the state of the game (ended / victory).  
 <img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/merged.png" width="400">  


- **Heuristic score**   
  The bot calculates the 4 possible outcomes after performing each move separately [up, down, right, left]. On the results we perform an analysis that has proven to be the most optimal in terms of number of wins for 2048.  

  The general idea of the method is to assign points to linear paths in the board (for example, to 8 tracks) and to select the maximum score of the tracks as a score that represents the test. This method is usually applied in depth (some forward moves), although we did not do so for the purpose of the project.  

  We decided to focus on 2 linear tracks, as follows:

<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/heurostic.png" width="400">  


  For each path we calculate the score as follows:  
<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/eval1.png" width="150">  
<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/eval2.png" width="200">  

  When we chose r to be 0.5 (we rely on the analysis published in the next link).  
  For each step calculation, not only does the game logic follow the desired move (eg sliding to the right), but it also analyzes the game mode (ended / win) and the scoring analysis. Finally the move was chosen with the highest score.    
<img src="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/screenshots/heurostic2.png" width="400"> 


### More information
How to run etc in the following <a href="https://github.com/MaorAssayag/2048-Simulation-System-In-Erlang/blob/master/2048Erlang.pdf">pdf (Hebrew)</a>.


### Creators
*Maor Assayag*  
Computer Engineer, Ben-gurion University, Israel

*Refhael Shetrit*  
Computer Engineer, Ben-gurion University, Israel  

