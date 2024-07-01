%% Save outputs as csv
%  ===================
%  Authors: Rebecca Collins
%  Created: 13-Jul-2022
%  Last modified: 12-Jul-2022
%  --------------------------
%  DESCRIPTION
%  Script that takes the output of the runs of various BNG offset scenarios
%  and saves into csvs to be analysed in R where summary tables can be 
%  produced.
%  It requires matlab structured created running the script run_NEV.m and
%  saved in Section 4.
%  ========================================================================

%% (1) LOAD THE REQUIRED DATA
%  ==========================
clear
load('Output/UCL/Pollinator/local_bio_offset_sng.mat')
load('Output/UCL/Pollinator/max_bio_offset_sng.mat')
load('Output/UCL/Pollinator/max_es_offset_sng.mat')
load('Output/UCL/Pollinator/max_netES_offset_sng.mat')
load('Output/UCL/Pollinator/max_equity_offset_sng.mat')
load('Output/UCL/Pollinator/min_cost_offset_sng.mat')

%% (2) SAVE ALL TABLES AS CSV
%  ==========================

% local offset 
% ------------
local_offset_benefits = local_bio_offset_sng.benefits;
local_offset_benefits.new2kid = local_bio_offset_sng.new2kid;
local_offset_benefits.hectares_chg = local_bio_offset_sng.hectares_chg; 
writetable(local_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\local_offset_benefits_sng.csv");

local_offset_costs = local_bio_offset_sng.costs;
local_offset_costs.new2kid = local_bio_offset_sng.new2kid;
local_offset_costs.hectares_chg = local_bio_offset_sng.hectares_chg; 
writetable(local_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\local_offset_costs_sng.csv");

local_offset_env_outs = local_bio_offset_sng.env_outs;
local_offset_env_outs.new2kid = local_bio_offset_sng.new2kid;
local_offset_env_outs.hectares_chg = local_bio_offset_sng.hectares_chg; 
writetable(local_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\local_offset_env_outs_sng.csv");

local_offset_es_outs = local_bio_offset_sng.es_outs;
local_offset_es_outs.new2kid = local_bio_offset_sng.new2kid;
local_offset_es_outs.hectares_chg = local_bio_offset_sng.hectares_chg; 
writetable(local_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\local_offset_es_outs_sng.csv");

% Max. biodiversity 
% -----------------
max_bio_offset_benefits = max_bio_offset_sng.benefits;
max_bio_offset_benefits.new2kid = max_bio_offset_sng.new2kid;
max_bio_offset_benefits.hectares_chg = max_bio_offset_sng.hectares_chg; 
writetable(max_bio_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_bio_offset_benefits_sng.csv");

max_bio_offset_costs = max_bio_offset_sng.costs;
max_bio_offset_costs.new2kid = max_bio_offset_sng.new2kid;
max_bio_offset_costs.hectares_chg = max_bio_offset_sng.hectares_chg; 
writetable(max_bio_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_bio_offset_costs_sng.csv");

max_bio_offset_env_outs = max_bio_offset_sng.env_outs;
max_bio_offset_env_outs.new2kid = max_bio_offset_sng.new2kid;
max_bio_offset_env_outs.hectares_chg = max_bio_offset_sng.hectares_chg; 
writetable(max_bio_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_bio_offset_env_outs_sng.csv");

max_bio_offset_es_outs = max_bio_offset_sng.es_outs;
max_bio_offset_es_outs.new2kid = max_bio_offset_sng.new2kid;
max_bio_offset_es_outs.hectares_chg = max_bio_offset_sng.hectares_chg; 
writetable(max_bio_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_bio_offset_es_outs_sng.csv");

% Max. ecosystem services 
% -----------------------
max_es_offset_benefits = max_es_offset_sng.benefits;
max_es_offset_benefits.new2kid = max_es_offset_sng.new2kid;
max_es_offset_benefits.hectares_chg = max_es_offset_sng.hectares_chg; 
writetable(max_es_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_es_offset_benefits_sng.csv");

max_es_offset_costs = max_es_offset_sng.costs;
max_es_offset_costs.new2kid = max_es_offset_sng.new2kid;
max_es_offset_costs.hectares_chg = max_es_offset_sng.hectares_chg; 
writetable(max_es_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_es_offset_costs_sng.csv");

max_es_offset_env_outs = max_es_offset_sng.env_outs;
max_es_offset_env_outs.new2kid = max_es_offset_sng.new2kid;
max_es_offset_env_outs.hectares_chg = max_es_offset_sng.hectares_chg; 
writetable(max_es_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_es_offset_env_outs_sng.csv");

max_es_offset_es_outs = max_es_offset_sng.es_outs;
max_es_offset_es_outs.new2kid = max_es_offset_sng.new2kid;
max_es_offset_es_outs.hectares_chg = max_es_offset_sng.hectares_chg; 
writetable(max_es_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_es_offset_es_outs_sng.csv");

% Max. net ecosystem services
% ----------------------------
max_netES_offset_benefits = max_netES_offset_sng.benefits;
max_netES_offset_benefits.new2kid = max_netES_offset_sng.new2kid;
max_netES_offset_benefits.hectares_chg = max_netES_offset_sng.hectares_chg; 
writetable(max_netES_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_netES_offset_benefits_sng.csv");

max_netES_offset_costs = max_netES_offset_sng.costs;
max_netES_offset_costs.new2kid = max_netES_offset_sng.new2kid;
max_netES_offset_costs.hectares_chg = max_netES_offset_sng.hectares_chg; 
writetable(max_netES_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_netES_offset_costs_sng.csv");

max_netES_offset_env_outs = max_netES_offset_sng.env_outs;
max_netES_offset_env_outs.new2kid = max_netES_offset_sng.new2kid;
max_netES_offset_env_outs.hectares_chg = max_netES_offset_sng.hectares_chg; 
writetable(max_netES_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_netES_offset_env_outs_sng.csv");

max_netES_offset_es_outs = max_netES_offset_sng.es_outs;
max_netES_offset_es_outs.new2kid = max_netES_offset_sng.new2kid;
max_netES_offset_es_outs.hectares_chg = max_netES_offset_sng.hectares_chg; 
writetable(max_netES_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_netES_offset_es_outs_sng.csv");

% % Max. recreation 
% % ---------------
% max_rec_offset_benefits = max_rec_offset_urban_sprawl_sng.benefits;
% max_rec_offset_benefits.new2kid = max_rec_offset_urban_sprawl_sng.new2kid;
% max_rec_offset_benefits.hectares_chg = max_rec_offset_urban_sprawl_sng.hectares_chg; 
% writetable(max_rec_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_benefits_sng.csv");
% 
% max_rec_offset_costs = max_rec_offset_urban_sprawl_sng.costs;
% max_rec_offset_costs.new2kid = max_rec_offset_urban_sprawl_sng.new2kid;
% max_rec_offset_costs.hectares_chg = max_rec_offset_urban_sprawl_sng.hectares_chg; 
% writetable(max_rec_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_costs_sng.csv");
% 
% max_rec_offset_env_outs = max_rec_offset_urban_sprawl_sng.env_outs;
% max_rec_offset_env_outs.new2kid = max_rec_offset_urban_sprawl_sng.new2kid;
% max_rec_offset_env_outs.hectares_chg = max_rec_offset_urban_sprawl_sng.hectares_chg; 
% writetable(max_rec_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_env_outs_sng.csv");
% 
% max_rec_offset_es_outs = max_rec_offset_urban_sprawl_sng.es_outs;
% max_rec_offset_es_outs.new2kid = max_rec_offset_urban_sprawl_sng.new2kid;
% max_rec_offset_es_outs.hectares_chg = max_rec_offset_urban_sprawl_sng.hectares_chg; 
% writetable(max_rec_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_es_outs_sng.csv");
% 
% Max. recreation equity weighted
% -------------------------------
max_rec_equity_weighted_offset_benefits = max_equity_offset_sng.benefits;
max_rec_equity_weighted_offset_benefits.new2kid = max_equity_offset_sng.new2kid;
max_rec_equity_weighted_offset_benefits.hectares_chg = max_equity_offset_sng.hectares_chg; 
writetable(max_rec_equity_weighted_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_rec_equity_weighted_offset_benefits_sng.csv");

max_rec_equity_weighted_offset_costs = max_equity_offset_sng.costs;
max_rec_equity_weighted_offset_costs.new2kid = max_equity_offset_sng.new2kid;
max_rec_equity_weighted_offset_costs.hectares_chg = max_equity_offset_sng.hectares_chg; 
writetable(max_rec_equity_weighted_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_rec_equity_weighted_offset_costs_sng.csv");

max_rec_equity_weighted_offset_env_outs = max_equity_offset_sng.env_outs;
max_rec_equity_weighted_offset_env_outs.new2kid = max_equity_offset_sng.new2kid;
max_rec_equity_weighted_offset_env_outs.hectares_chg = max_equity_offset_sng.hectares_chg; 
writetable(max_rec_equity_weighted_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_rec_equity_weighted_offset_env_outs_sng.csv");

max_rec_equity_weighted_offset_es_outs = max_equity_offset_sng.es_outs;
max_rec_equity_weighted_offset_es_outs.new2kid = max_equity_offset_sng.new2kid;
max_rec_equity_weighted_offset_es_outs.hectares_chg = max_equity_offset_sng.hectares_chg; 
writetable(max_rec_equity_weighted_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_rec_equity_weighted_offset_es_outs_sng.csv");

% Min cost
% --------
min_cost_offset_benefits = min_cost_offset_sng.benefits;
min_cost_offset_benefits.new2kid = min_cost_offset_sng.new2kid;
min_cost_offset_benefits.hectares_chg = min_cost_offset_sng.hectares_chg; 
writetable(min_cost_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\min_cost_offset_benefits_sng.csv");

min_cost_offset_costs = min_cost_offset_sng.costs;
min_cost_offset_costs.new2kid = min_cost_offset_sng.new2kid;
min_cost_offset_costs.hectares_chg = min_cost_offset_sng.hectares_chg; 
writetable(min_cost_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\min_cost_offset_costs_sng.csv");

min_cost_offset_env_outs = min_cost_offset_sng.env_outs;
min_cost_offset_env_outs.new2kid = min_cost_offset_sng.new2kid;
min_cost_offset_env_outs.hectares_chg = min_cost_offset_sng.hectares_chg; 
writetable(min_cost_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\min_cost_offset_env_outs_sng.csv");

min_cost_offset_es_outs = min_cost_offset_sng.es_outs;
min_cost_offset_es_outs.new2kid = min_cost_offset_sng.new2kid;
min_cost_offset_es_outs.hectares_chg = min_cost_offset_sng.hectares_chg; 
writetable(min_cost_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\min_cost_offset_es_outs_sng.csv");

% % Max pollinators
% % ---------------
% max_pollinator_offset_benefits = max_pollinator_offset_sng.benefits;
% max_pollinator_offset_benefits.new2kid = max_pollinator_offset_sng.new2kid;
% max_pollinator_offset_benefits.hectares_chg = max_pollinator_offset_sng.hectares_chg; 
% writetable(max_pollinator_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_pollinator_offset_benefits_sng.csv");
% 
% max_pollinator_offset_costs = max_pollinator_offset_sng.costs;
% max_pollinator_offset_costs.new2kid = max_pollinator_offset_sng.new2kid;
% max_pollinator_offset_costs.hectares_chg = max_pollinator_offset_sng.hectares_chg; 
% writetable(max_pollinator_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_pollinator_offset_costs_sng.csv");
% 
% max_pollinator_offset_env_outs = max_pollinator_offset_sng.env_outs;
% max_pollinator_offset_env_outs.new2kid = max_pollinator_offset_sng.new2kid;
% max_pollinator_offset_env_outs.hectares_chg = max_pollinator_offset_sng.hectares_chg; 
% writetable(max_pollinator_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_pollinator_offset_env_outs_sng.csv");
% 
% max_pollinator_offset_es_outs = max_pollinator_offset_sng.es_outs;
% max_pollinator_offset_es_outs.new2kid = max_pollinator_offset_sng.new2kid;
% max_pollinator_offset_es_outs.hectares_chg = max_pollinator_offset_sng.hectares_chg; 
% writetable(max_pollinator_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_pollinator_offset_es_outs_sng.csv");
% 
% % Max priority
% % ------------
% max_priority_offset_benefits = max_priority_offset_sng.benefits;
% max_priority_offset_benefits.new2kid = max_priority_offset_sng.new2kid;
% max_priority_offset_benefits.hectares_chg = max_priority_offset_sng.hectares_chg; 
% writetable(max_priority_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\UCL\Pollinator\max_priority_offset_benefits_sng.csv");
% 
% max_priority_offset_costs = max_priority_offset_sng.costs;
% max_priority_offset_costs.new2kid = max_priority_offset_sng.new2kid;
% max_priority_offset_costs.hectares_chg = max_priority_offset_sng.hectares_chg; 
% writetable(max_priority_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_priority_offset_costs_sng.csv");
% 
% max_priority_offset_env_outs = max_priority_offset_sng.env_outs;
% max_priority_offset_env_outs.new2kid = max_priority_offset_sng.new2kid;
% max_priority_offset_env_outs.hectares_chg = max_priority_offset_sng.hectares_chg; 
% writetable(max_priority_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_priority_offset_env_outs_sng.csv");
% 
% max_priority_offset_es_outs = max_priority_offset_sng.es_outs;
% max_priority_offset_es_outs.new2kid = max_priority_offset_sng.new2kid;
% max_priority_offset_es_outs.hectares_chg = max_priority_offset_sng.hectares_chg; 
% writetable(max_priority_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_priority_offset_es_outs_sng.csv");
