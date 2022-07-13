%% run_NEV.m
%  =========
% Author: Mattia Mancini, Rebecca Collins
% Created: 25 Feb 2022
% Last modified: 22 Jul 2022
% ---------------------------------------
% DESCRIPTION
% Script to run the NEV tool for scenario analysis of changes in ecosystem
% services as a result to changes in land use. 
% It requires a baseline land use and a land use that differs from the
% baseline in order to compute changes in ecosystem services. For the NERC
% Agile-Sprint the baseline is the land uses from the 2020 land cover map
% for the BBOWT - NEP area along the Oxford-Cambridge corridor. This has
% been produced in the script 'calc_baseline_landuses.R'. Scenarios need to
% be passed as land uses in the same format as the baseline landuse.
%
% INPUTS
%   1) Model parameters
%   2) flags
%   3) Land uses
%      For the models to all work the landuse table must
%      contain the following columns (the order does NOT matter):
%      1) new2kid; 2) urban_ha; 3) sngrass_ha; 4) wood_ha; 5) farm_ha; 
%      6) water_ha. As farm is the sum of arable_ha and grass_ha, if those
%      are passed, farm_ha is no longer required. The same applies for
%      water_ha, i.e. the sum of freshwater_ha, marine_ha, coast_ha and
%      ocean_ha.
%      NB: more columns can be passed, to override specific default
%      landuses:
%      - wood_mgmt_ha can be passed to override the default values in the
%        NEV database. The default proportions of coniferous and
%        deciduous woodland will still be applied, as well as the
%        proportions for non managed woodland. 
%         -------   AAA   ------ 
%        if new woodland is created in a scenario, this must be managed for 
%        the correct calculation of GHGs. If wood_mgmt_ha is passed in the 
%        scenario land use, but new woodland from baseline in the scenario 
%        is more than the woodland in wood_mgmt_ha (i.e. 
%        wood_mgmt_ha(scenario) < (wood_ha(scenario) - wood_ha(baseline))
%        then the script will overwrtie the wood_mgmt_ha passed and replace 
%        it with the hectares of new woodland (i.e. wood_mgmt_ha(scenario) 
%        = (wood_ha(scenario) - wood_ha(baseline))
%      - it is possible to override the top-level farm model that allocates
%        land between arable and farm grassland, passing values of grass_ha
%        (farm grassland) and arable_ha. In this case the farm model will
%        still allocate arable between the various crop types and grassland
%        between grassland types. To do this, grass_ha and arable_ha values
%        must be passed, and the model_flags.run_ag_toplevel must be set to
%        false. 
%
%        !!!!!!!!!  AAA  !!!!!!!!
%        scenario land uses assume that the land use change
%        passed happens in YEAR 1 !!!!!!!
% =========================================================================

%% (0) Set up
%      (a) model parameters and selection of modules to run
%  ========================================================
clear
addpath(genpath('D:\Documents\GitHub\Run_NEV_Scenarios\'));

% 1.1. model parameters related to land uses and farm model
% ---------------------------------------------------------
parameters = fcn_set_parameters();
parameters.parent_dir       = 'D:\Documents\NEV\Model Data\';
parameters.lcm_data_folder  = 'D:\Documents\Data\BNG\Data\LCM\LCM_2km\';
parameters.num_years                   = 40;
parameters.start_year                  = 2020;
parameters.clim_string                 = 'ukcp18';
parameters.clim_scen_string            = 'rcp60';
parameters.temp_pct_string             = '50';
parameters.rain_pct_string             = '50';
parameters.biodiversity_climate_string = 'current';
parameters.other_ha                    = 'baseline'; 
parameters.landuse_change_timeframe    = 50; % land use change remains for these numbers of years

% 1.2. Model parameters for valuation
% -----------------------------------
parameters.assumption_flooding = 'low';
parameters.assumption_nonuse = 0.38; % this could be 0.38, 0.75, 1 
parameters.assumption_pop = 'low';

% 1.3. Land use changes allowed
% -----------------------------
parameters.options = {'arable2sng', 'arable2wood', 'arable2urban', 'arable2mixed', ...
                      'grass2sng', 'grass2wood', 'grass2urban', 'grass2mixed', ...
                      'sng2urban', 'wood2urban'};


% 1.3. Flags to select which models to run
% ----------------------------------------
model_flags.run_ag_toplevel   = true;
model_flags.run_ghg           = true;
model_flags.run_forestry      = true;
model_flags.run_biodiversity  = true;
model_flags.run_hydrology     = true;
model_flags.run_recreation    = true;



%% (2) LOAD LAND USES
%      2.1 - Load baseline land use data
%      2.2 - Run the scenario land use data
%  ========================================

% 2.1. Load baseline land uses. This is either a land cover map from CEH,
%      or a modification of one of the CEH LCMs. When passing a land cover
%      table, we also need to specify which CEH LCM it originates from in
%      order to correclty calculate baselines for each of the NEV modules.
% ------------------------------------------------------------------------
base_ceh_lcm = '2000';
landuse_data_path = 'D:\Documents\Data\BNG\Data\Urban Sprawl - F.Eigenbrod\';
baseline_lu = readtable(strcat(landuse_data_path, 'urban_sprawl_2031_sprawl.csv'));
parameters.base_ceh_lcm = base_ceh_lcm;

% 2.2. Load scenario land use
% ---------------------------
scenario_lu = baseline_lu;
scenario_lu.sng_ha = scenario_lu.sng_ha + 0.5 .* scenario_lu.farm_ha;
scenario_lu.wood_ha = scenario_lu.wood_ha + 0.5 .* scenario_lu.farm_ha;
scenario_lu.farm_ha = zeros(height(scenario_lu), 1);
% scenario_lu = baseline_lu;
% landuse_data_path = 'D:\Documents\GitHub\BNG\Output\';
% scenario_lu_eng = readtable(strcat(landuse_data_path, 'max_es_offset_urban_sprawl_equity_weighted.csv'));
% scenario_lu_eng.offset_area_ha = [];
% [~, idx] = ismember(scenario_lu.Properties.VariableNames, scenario_lu_eng.Properties.VariableNames); 
% scenario_lu_eng = scenario_lu_eng(:,idx);
% 
% [~, idx] = intersect(scenario_lu.new2kid, scenario_lu_eng.new2kid);
% scenario_lu(idx,:) = scenario_lu_eng;

                                    
%% (3) RUN THE MODELS
%  ==================
[benefits, costs, env_outs, es_outs] = fcn_run_scenario(model_flags, ...
                            parameters, ...
                            baseline_lu, ... 
                            scenario_lu);

% remove NaNs
benefits = fillmissing(benefits, 'constant', 0);
costs = fillmissing(costs, 'constant', 0);
env_outs = fillmissing(env_outs, 'constant', 0);
es_outs = fillmissing(es_outs, 'constant', 0);
hectares_chg = baseline_lu.farm_ha;

%% (4) SAVE THE OUTPUT
%  ===================
all_farm2mixed_sprawl_2031 = struct('benefits', benefits, ...
                                    'costs', costs, ...
                                    'env_outs', env_outs, ...
                                    'es_outs', es_outs, ...
                                    'hectares_chg', hectares_chg, ...
                                    'new2kid', baseline_lu.new2kid);
                               
save('Output/all_farm2mixed_sprawl_2031', 'all_farm2mixed_sprawl_2031')


%% (5) SCENARIO SPECIFIC OUTPUT
%  ============================
%  CHANGE THIS BASED ON NEEDS

% 5.1. Biodiverity: used for the identification of offset locations that
%      maximise biodiversity improvements
% ----------------------------------------------------------------------
biodiversity_chg = array2table([baseline_lu.new2kid, ...
                               env_outs.bio, ...
                               env_outs.bio ./ hectares_chg]);
biodiversity_chg = fillmissing(biodiversity_chg, 'constant', 0);
biodiversity_chg.Properties.VariableNames = {'new2kid', 'sr_chg_perc', 'sr_chg_ha'};
writetable(biodiversity_chg, 'Output/all_farm2mixed_bio_sprawl_2031.csv');

% 5.2. Ecosystem services: used for the identification of offset locations 
%      that maximise ES improvements
% ------------------------------------------------------------------------
tot_es = sum(es_outs{:,1:5}, 2);
es_chg = array2table([baseline_lu.new2kid, ...
                      tot_es, ...
                      tot_es ./ hectares_chg]);
es_chg = fillmissing(es_chg, 'constant', 0);
es_chg.Properties.VariableNames = {'new2kid', 'tot_es', 'tot_es_ha'};
writetable(es_chg, 'Output/all_farm2mixed_tot_es_sprawl_2031.csv');

% 5.3. Ecosystem services, for equity weighting
% ---------------------------------------------
tot_es = [array2table(baseline_lu.new2kid), es_outs(:,1:5)]; 
tot_es.hectares_chg = hectares_chg;
tot_es.Properties.VariableNames(1) = {'new2kid'};
writetable(tot_es, 'Output/all_farm2mixed_all_es_sprawl_2031.csv');
