
# csmbuilder v0.1.0.9002

- Add n_dim argument to csm_create_data_structure() for specifying the dimensions
  of a data structure

# csmbuilder v0.1.0.9001

- Adding Hill equation utility functions for up/down-regulation:
  - csm_hill_up_reg()
  - csm_hill_down_reg()
- Adding a function for running a group of simulations: csm_run_sim_group()

# csmbuilder v0.1.0

- Release of first complete version of package!

- This version provides functions that can:
    - define state variables, parameters, input variables and data structures
    - combine variables, parameters and data structure definitions into a model
    - render a model as R code or as an R function object
    - run a simulation with the rendered model
