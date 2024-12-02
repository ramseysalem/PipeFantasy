# README for Pipe Fantasy

## Overview

**Pipe Fantasy** is an interactive game implemented in Racket using the `2htdp/image` and `2htdp/universe` libraries. The game revolves around building a pipeline system on a grid and guiding goo through it to score points. The player strategically places pipes to keep the goo flowing while avoiding dead ends.

---

## Features

### Core Concepts
- **Pipes**: Defined by their openings (top, bottom, left, and right).
- **Starting Pipe**: The initial pipe where the goo begins its journey.
- **Grid**: A flexible n x n grid where pipes are placed.
- **Goo Flow**: Tracks the current path of the goo through connected pipes.
- **Score**: Points are calculated based on the length of the goo flow and the efficiency of pipe placement.

### Pipe Types
- **Basic Pipes**: Include T-shaped, L-shaped, straight, and fully connected pipes.
- **Starting Pipes**: Pipes with only one opening, determining the initial flow direction.

---

## Gameplay Mechanics

1. **Placing Pipes**:
   - Players place pipes on a grid using mouse clicks.
   - The game ensures valid placements based on the grid and the current game state.

2. **Goo Propagation**:
   - Goo flows from the starting pipe and traverses connected openings.
   - Dead ends or disconnected pipes stop the goo flow.

3. **Score Calculation**:
   - Players earn points for maintaining continuous goo flow.
   - Avoid replacing pipes unnecessarily, as it incurs penalties.

4. **Grid Updates**:
   - Each tick, goo automatically propagates forward if possible.
   - Players must act quickly to extend the pipeline before the goo stops.

---

## Game Components

### Structures
1. **Pipe**: Defines pipe properties (openings in each direction).
2. **PipeWithCoords**: Represents a pipe's position on the grid.
3. **Grid**: Encapsulates the grid size and placed pipes.
4. **GooFlow**: Tracks the path of the goo.
5. **GameState**: Contains all the game's essential data, including grid, incoming pipes, goo flow, and score.

### Key Functions
- **`pipe->image`**: Renders a pipe as an image.
- **`place-pipe`**: Places a pipe on the grid.
- **`grid-goo-propagate`**: Advances the goo along the connected pipes.
- **`fit?`**: Checks if two pipes connect properly.
- **`tick`**: Advances the game state by one step.
- **`get-score`**: Computes the player's score based on the goo flow and replaced pipes.

### Interactive Gameplay
- **`pipe-fantasy`**: Starts the game loop using `big-bang`, enabling interactive play.

---

## How to Play

1. **Initialize the Game**:
   - Run `(pipe-fantasy GS-5)` to start the game with the default configuration.

2. **Place Pipes**:
   - Click on the grid to place incoming pipes. Pipes must align with existing goo paths.

3. **Guide the Goo**:
   - Ensure goo flows smoothly by strategically placing pipes.
   - Use the incoming pipe list wisely.

4. **Score Points**:
   - Build efficient paths to maximize your score.
   - Avoid unnecessary replacements.

5. **Game Over**:
   - The game ends if the goo cannot flow further.

---

## Requirements

- Racket programming environment.
- Libraries: `2htdp/image`, `2htdp/universe`.

