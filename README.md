# STRADS-CLAUDE

## Simulation of Temporally Recursive Agents with Dynamic Scenarios - Collaborative Learning and Understanding through Dynamic Environments

STRADS-CLAUDE is an advanced simulation system inspired by Douglas Lenat's STRADS (STRategic Automatic Discovery System). This project aims to create a flexible, knowledge-based simulation of possible futures, allowing for the exploration of complex scenarios and agent interactions.

### Features

- Dynamic event generation and processing
- Multiple agents with unique personalities and behaviors
- Complex world state management
- Belief system for agents
- Customizable reactions and outcomes
- Logging system with multiple levels (debug, info, warning, error)
- Natural Language Generation (NLG) for creating narrative summaries of simulations

### Getting Started

#### Prerequisites

- SWI-Prolog (version 8.0 or higher recommended)

#### Installation

1. Clone the repository:

`git clone https://github.com/yourusername/strads-claude.git`

2. Navigate to the project directory:

`cd strads-claude`

#### Running the Simulation

To run the simulation, use the following command in your terminal:

`swipl -s simulation.pl`

This will start the simulation and output the results, including the debug information and the generated "Annals" at the end.

### Customizing the Simulation

You can customize various aspects of the simulation by modifying the following components:

- `atomic_personality/2`: Define personalities for agents
- `atomic_reaction/2`: Specify possible reactions to events
- `atomic_outcome/2`: Define outcomes for specific actions
- `atomic_affect/2`: Determine which actors are affected by specific events
- `atomic_belief_update/2`: Specify how beliefs are updated based on actions

### Understanding the Output

The simulation output includes:

1. Detailed logs of event processing, world state changes, and agent interactions
2. A summary of each simulation cycle, including world state and actor beliefs
3. "The Annals of Our Simulation": A narrative summary of key events in simple English

### Contributing

Contributions to STRADS-CLAUDE are welcome! Please feel free to submit pull requests, create issues, or suggest improvements.

### License

This project is licensed under the GPLv3  License - see the [LICENSE.md](LICENSE.md) file for details.

### Acknowledgments

- Inspired by Douglas Lenat's original STRADS concept
- Developed with the assistance of Anthropic's Claude 3.5 Sonnet LLM
