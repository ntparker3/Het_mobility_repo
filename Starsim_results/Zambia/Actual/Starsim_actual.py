import numpy as np
import starsim as ss
import sciris as sc
import pandas as pd
from enum import IntEnum, auto
import seaborn as sns
from pathlib import Path
import pickle
from collections import defaultdict


###### function to run simulations of multiple location datasets

###### Arguments:
### location_data_folder: path/folder name with location date (i.e. "Actual_loc_10")
### output_name: desired name for output. Need to have .pkl at end because it's a pickle object (i.e. "Lusaka_COVID_sims.pkl")
### sim_length: end day of simulation (i.e. 299)
### duration_of_exp: duration of exposure period as a distribution (i.e. ss.lognorm_ex(mean=0.4, std=0.3))
### ORIGIN: starting location of outbreak. Always captialized and no parenthesis (i.e. LUSAKA)
### size_of_init_outbreak: number of people initially infected (i.e. 5)
### beta_val: beta value of pathogen (i.e. 0.0577)
### duration_of_infectiousness: duration of infectious period as a distribution (i.e. ss.lognorm_ex(mean=0.4, std=0.3))


def starsim_sim_function(location_data_folder, output_name, sim_length, duration_of_exp, ORIGIN, size_of_init_outbreak, beta_val, duration_of_infectiousness): 
    
    ### first, define all arguments 
    location_data_folder = location_data_folder
    output_name = output_name
    sim_length = sim_length
    duration_of_exp = duration_of_exp
    ORIGIN = ORIGIN
    size_of_init_outbreak = size_of_init_outbreak
    beta_val = beta_val
    duration_of_infectiousness = duration_of_infectiousness

    ### create LocationDemographics class to allow for dynamically updating location data
    class LocationDemographics(ss.Demographics):
        """ Track and update each person's location at every timestep """
        def __init__(self, location_dataset, locations=None, pars=None, **kwargs):
            """
            Initialize locations and movement probabilities.

            Args:
                locations (list): List of possible locations.
                movement_probs (dict): Dictionary defining movement probabilities between locations.
            """
            super().__init__()
            
            self.define_pars(
                locations=locations , 
                location_dataset=location_dataset
                
            )
            self.update_pars(pars, **kwargs)
            
            return
        
        def init_pre(self, sim):
            """ 
            Initialize the location for each person at the start of the simulation 
            """
            super().init_pre(sim)
            people = self.sim.people
            loc_data = self.pars.location_dataset
            print("Number of people:", len(self.sim.people))


            people.location = loc_data[(loc_data["step"] == 0)]["location"].values
            print("peoples.location:", people.location)
            
            return

            
        
        def step(self):
            """ Update each person's location at every timestep """
            sim = self.sim
            people = self.sim.people
            loc_data = self.pars.location_dataset

            if self.ti % 5 == 0:
                print("time step", self.ti)

            
            current_step_data = loc_data[loc_data["step"] == self.ti][["uid", "location"]]


            location_array = current_step_data["location"].values
            people.location[:] = location_array
            
            return

        def init_results(self):
            """ Initialize tracking of locations over time """
            super().init_results()
            for loc in self.pars.locations:
                self.define_results(ss.Result(f'num_in_{loc}', dtype=int, label=f'People in {loc}'))
            return
        
        def update_results(self):
            """ Store location data at each timestep """
            people = self.sim.people
            for loc in self.pars.locations:
                self.results[f'num_in_{loc}'][self.ti] = np.sum(people.location == loc)
            return


    ### set the parameters of the simulation. The length of the simulation is adjustable.     
    pars = sc.objdict(start = 0, stop= sim_length, unit = 'day',
                    verbose = 0)

    n_people = 20000


    ### load in the district data and set the LOCATION variable, which assigns a unqiue number to each location 
    districts = pd.read_csv("/home/nparke19/Zambia/Starsim/district_names.csv")

    district_names = districts["District"].tolist()

    LOCATION = IntEnum("LOCATION", {name.upper().replace(" ", "_"): i for i, name in enumerate(district_names)})



    ### create a CustomNet class which defines the network structure manually
    class CustomNet(ss.Network):
        def __init__(self, contact_dict):
            super().__init__()
            self.contact_dict = contact_dict

        def step(self):
            contacts = self.contact_dict[self.ti]
            self.edges.p1 = contacts['p1']
            self.edges.p2 = contacts['p2']
            self.edges.beta = np.ones(len(self))
            self.validate()



    ### create SEIR class, which codes in the structure and dynamics of our your disease.
    ### This needs duration of exposure (distribution) argument. 
    class SEIR(ss.SIR):
        def __init__(self, pars=None, *args, **kwargs):
            super().__init__()
            self.define_pars(dur_exp = duration_of_exp
            )
            self.update_pars(pars, **kwargs)

            # Additional states beyond the SIR ones
            self.define_states(
                ss.State('exposed', label='Exposed'),
                ss.FloatArr('ti_exposed', label='TIme of exposure'),
            )
            return

        @property
        def infectious(self):
            return self.infected 

        def step_state(self):
            """ Make all the updates from the SIR model """
            # Perform SIR updates
            super().step_state()

            # Additional updates: progress exposed -> infected
            infected = self.exposed & (self.ti_infected <= self.ti)
            self.exposed[infected] = False
            self.infected[infected] = True
            return

        def step_die(self, uids):
            super().step_die(uids)
            self.exposed[uids] = False
            return

        def set_prognoses(self, uids, sources=None):
            """ Carry out state changes associated with infection """
            super().set_prognoses(uids, sources)
            ti = self.ti
            self.susceptible[uids] = False
            self.exposed[uids] = True
            self.ti_exposed[uids] = ti

            # Calculate and schedule future outcomes
            dur_exp = self.pars['dur_exp'].rvs(uids)
            self.ti_infected[uids] = ti + dur_exp
            dur_inf = self.pars['dur_inf'].rvs(uids)
            will_die = self.pars['p_death'].rvs(uids)
            self.ti_recovered[uids[~will_die]] = ti + dur_inf[~will_die]
            self.ti_dead[uids[will_die]] = ti + dur_inf[will_die]
            return

        def plot(self):
            """ Update the plot with the exposed compartment """
            with ss.options.context(jupyter=False):
                fig = super().plot()
                ax = plt.gca()
                res = self.results.n_exposed
                ax.plot(res.timevec, res, label=res.label)
                plt.legend()
            return ss.return_fig(fig)
        
    LOCATION = IntEnum("LOCATION", {name.upper().replace(" ", "_"): i for i, name in enumerate(district_names)})
    LOCATIONS = list(LOCATION)

    ### define the NewInfectionsByLocation_SEIR analyzer, which returns the number of new infections per step in each location 
    class NewInfectionsByLocation_SEIR(ss.Analyzer):
        def init_results(self):
            """Initialize storage for infection counts per location."""
            self.new_cases = np.zeros((len(self), len(LOCATIONS)))

        def step(self):
            """Record new infections by location at each time step."""
            new_inf = np.ceil(self.sim.diseases.seir.ti_infected) == self.ti
            if not new_inf.any(): return

            # Vectorized counting using np.bincount()
            loc_indices = self.sim.people.location[new_inf]  # Get locations of new infections
            counts = np.bincount(loc_indices, minlength=len(LOCATIONS))  # Count per location
            

            # Store results
            self.new_cases[self.ti, :] = counts
        def finalize(self):
            """Store results as a Pandas DataFrame."""
            # Convert new_cases array to a DataFrame
            self.results_df = pd.DataFrame(self.new_cases, columns=LOCATIONS)


    ### Define the simulate_function, which puts everything together and runs the simulation
    ### Needs multiple arguments: file_path of the location data,  
    def simulate_function(location_data_path):
        
        #load in the dataset and transform locations into numbers. 
        location_sim= pd.read_csv(location_data_path)
        location_sim = pd.DataFrame(location_sim)
        location_sim["location"] = location_sim["location"].apply(lambda x: int(LOCATION[x.upper().replace(" ", "_")]))
        location_sim = location_sim[location_sim["step"] <= pars['stop']]

        # Create the People object
        location_arr= ss.FloatArr("location", default=location_sim[location_sim["step"] == 0]["location"].values)
        ppl_return = ss.People(n_agents=n_people, extra_states=location_arr)

        origin_enum = getattr(LOCATION, ORIGIN)

        def seeding_return(self, sim, uids):
            p = np.zeros(len(uids))
            loc_0 = ss.uids(location_sim[(location_sim["location"] == origin_enum) & (location_sim["step"] == 0)]["uid"].values)
            selected_uids = np.random.choice(loc_0, size = size_of_init_outbreak)
            p[selected_uids] = 1
            return p
        
        seir_disease_return = SEIR(init_prev = ss.bernoulli(p = seeding_return),
                                    beta = beta_val,
                                    dur_inf = duration_of_infectiousness, 
                                    p_death = 0)

        # create the contact network
        # Parameters
        from collections import defaultdict

        lambda_contacts = 5.03  # Mean number of contacts per person per day
        contacts_dict = {}

        # Group once by step and location
        grouped = location_sim.groupby(["step", "location"])

        for (step, location), group in grouped:
            agents = group["uid"].values
            n = len(agents)

            if n <= 1:
                continue

            # Estimate total number of edges: n * mean_contacts / 2
            expected_edges = int(n * lambda_contacts / 2)
            
            # Sample contact pairs
            idx1 = np.random.choice(n, size=expected_edges, replace=True)
            idx2 = np.random.choice(n, size=expected_edges, replace=True)

            # Remove self-contacts
            mask = idx1 != idx2
            pairs = np.stack([agents[idx1[mask]], agents[idx2[mask]]], axis=1)

            # Sort each pair to avoid (a,b) vs (b,a)
            pairs = np.sort(pairs, axis=1)

            # Remove duplicate pairs
            pairs = np.unique(pairs, axis=0)

            # Initialize the step entry if not yet added
            if step not in contacts_dict:
                contacts_dict[step] = {"p1": [], "p2": []}
            
            contacts_dict[step]["p1"].extend(pairs[:, 0])
            contacts_dict[step]["p2"].extend(pairs[:, 1])

        for step in contacts_dict:
            contacts_dict[step]["p1"] = np.array(contacts_dict[step]["p1"])
            contacts_dict[step]["p2"] = np.array(contacts_dict[step]["p2"])
        

        ### create the custom network based on the contacts dictionary
        custom_network_return = CustomNet(contacts_dict)

        ### make a list of all locations used in the travel data
        LOCATIONS = list(LOCATION)

        ### initialize the location demographic class and the analyzer
        demographics_sim = [LocationDemographics(location_dataset= location_sim, locations = LOCATIONS)]

        az = NewInfectionsByLocation_SEIR()

        ### run the sim
        simulation = ss.Sim(pars, diseases = seir_disease_return, people= ppl_return, networks=custom_network_return, demographics = demographics_sim, analyzers=[az])
        simulation.run()

        ### obtain and return the analyzer output
        new_infections_return_df = simulation.analyzers.newinfectionsbylocation_seir.results_df

        print(location_data_path)
        
        return new_infections_return_df

    ### repeat simulate_function for each location dataset in a folder
    total_sims = [simulate_function(file_path) for file_path in Path(location_data_folder).glob("*csv")]


    ### save list of dataframes in a pkl object, which can be opened in R
    with open(output_name, "wb") as f:
        pickle.dump(total_sims, f)


starsim_sim_function(location_data_folder= "/home/nparke19/Zambia/Sim_location/Sim_both_het/Sim_datasets",
                     output_name= "/home/nparke19/Zambia/Starsim/Both_het/Both_het_COVID_Kalulushi_sims.pkl",
                     sim_length=150,
                     ORIGIN= "KALULUSHI",
                     duration_of_exp=ss.lognorm_ex(mean = 1.6, std = 0.3),
                     duration_of_infectiousness=ss.poisson(7),
                     size_of_init_outbreak=5,
                     beta_val=0.077)

starsim_sim_function(location_data_folder= "/home/nparke19/Zambia/Sim_location/Sim_both_het/Sim_datasets",
                     output_name= "/home/nparke19/Zambia/Starsim/Both_het/Both_het_Flu_Kalulushi_sims.pkl",
                     sim_length=299,
                     ORIGIN= "KALULUSHI",
                     duration_of_exp=ss.lognorm_ex(mean = 0.4, std = 0.3),
                     duration_of_infectiousness=ss.normal(loc = 4.8, scale = 0.8),
                     size_of_init_outbreak=5,
                     beta_val=0.0542)

starsim_sim_function(location_data_folder= "/home/nparke19/Zambia/Sim_location/Sim_both_het/Sim_datasets",
                     output_name= "/home/nparke19/Zambia/Starsim/Both_het/Both_het_Measles_Kalulushi_sims.pkl",
                     sim_length=100,
                     ORIGIN= "KALULUSHI",
                     duration_of_exp=ss.lognorm_ex(mean = 2.5, std = 0.15),
                     duration_of_infectiousness=ss.lognorm_ex(mean = 2.4, std = 0.2),
                     size_of_init_outbreak=5,
                     beta_val=0.216)
