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
load('Output/local_bio_offset_scc.mat')
load('Output/max_bio_offset_scc.mat')
load('Output/max_es_offset_scc.mat')
load('Output/max_equity_offset_scc.mat')
load('Output/min_cost_offset_scc.mat')

%% (2) SAVE ALL TABLES AS CSV
%  ==========================

% local offset 
% ------------
local_offset_benefits = local_bio_offset.benefits;
local_offset_benefits.new2kid = local_bio_offset.new2kid;
local_offset_benefits.hectares_chg = local_bio_offset.hectares_chg; 
writetable(local_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\local_offset_benefits_scc.csv");

local_offset_costs = local_bio_offset.costs;
local_offset_costs.new2kid = local_bio_offset.new2kid;
local_offset_costs.hectares_chg = local_bio_offset.hectares_chg; 
writetable(local_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\local_offset_costs_scc.csv");

local_offset_env_outs = local_bio_offset.env_outs;
local_offset_env_outs.new2kid = local_bio_offset.new2kid;
local_offset_env_outs.hectares_chg = local_bio_offset.hectares_chg; 
writetable(local_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\local_offset_env_outs_scc.csv");

local_offset_es_outs = local_bio_offset.es_outs;
local_offset_es_outs.new2kid = local_bio_offset.new2kid;
local_offset_es_outs.hectares_chg = local_bio_offset.hectares_chg; 
writetable(local_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\local_offset_es_outs_scc.csv");

% Max. biodiversity 
% -----------------
max_bio_offset_benefits = max_bio_offset_scc.benefits;
max_bio_offset_benefits.new2kid = max_bio_offset_scc.new2kid;
max_bio_offset_benefits.hectares_chg = max_bio_offset_scc.hectares_chg; 
writetable(max_bio_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_bio_offset_benefits_scc.csv");

max_bio_offset_costs = max_bio_offset_scc.costs;
max_bio_offset_costs.new2kid = max_bio_offset_scc.new2kid;
max_bio_offset_costs.hectares_chg = max_bio_offset_scc.hectares_chg; 
writetable(max_bio_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_bio_offset_costs_scc.csv");

max_bio_offset_env_outs = max_bio_offset_scc.env_outs;
max_bio_offset_env_outs.new2kid = max_bio_offset_scc.new2kid;
max_bio_offset_env_outs.hectares_chg = max_bio_offset_scc.hectares_chg; 
writetable(max_bio_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_bio_offset_env_outs_scc.csv");

max_bio_offset_es_outs = max_bio_offset_scc.es_outs;
max_bio_offset_es_outs.new2kid = max_bio_offset_scc.new2kid;
max_bio_offset_es_outs.hectares_chg = max_bio_offset_scc.hectares_chg; 
writetable(max_bio_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_bio_offset_es_outs_scc.csv");

% Max. ecosystem services 
% -----------------------
max_es_offset_benefits = max_es_offset_scc.benefits;
max_es_offset_benefits.new2kid = max_es_offset_scc.new2kid;
max_es_offset_benefits.hectares_chg = max_es_offset_scc.hectares_chg; 
writetable(max_es_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_offset_benefits_scc.csv");

max_es_offset_costs = max_es_offset_scc.costs;
max_es_offset_costs.new2kid = max_es_offset_scc.new2kid;
max_es_offset_costs.hectares_chg = max_es_offset_scc.hectares_chg; 
writetable(max_es_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_offset_costs_scc.csv");

max_es_offset_env_outs = max_es_offset_scc.env_outs;
max_es_offset_env_outs.new2kid = max_es_offset_scc.new2kid;
max_es_offset_env_outs.hectares_chg = max_es_offset_scc.hectares_chg; 
writetable(max_es_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_offset_env_outs_scc.csv");

max_es_offset_es_outs = max_es_offset_scc.es_outs;
max_es_offset_es_outs.new2kid = max_es_offset_scc.new2kid;
max_es_offset_es_outs.hectares_chg = max_es_offset_scc.hectares_chg; 
writetable(max_es_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_offset_es_outs_scc.csv");

% % Max. ecosystem services equity weighted
% % ---------------------------------------
% max_es_equity_weighted_offset_benefits = max_es_offset_urban_sprawl_equity_weighted_scc.benefits;
% max_es_equity_weighted_offset_benefits.new2kid = max_es_offset_urban_sprawl_equity_weighted_scc.new2kid;
% max_es_equity_weighted_offset_benefits.hectares_chg = max_es_offset_urban_sprawl_equity_weighted_scc.hectares_chg; 
% writetable(max_es_equity_weighted_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_equity_weighted_offset_benefits_scc.csv");
% 
% max_es_equity_weighted_offset_costs = max_es_offset_urban_sprawl_equity_weighted_scc.costs;
% max_es_equity_weighted_offset_costs.new2kid = max_es_offset_urban_sprawl_equity_weighted_scc.new2kid;
% max_es_equity_weighted_offset_costs.hectares_chg = max_es_offset_urban_sprawl_equity_weighted_scc.hectares_chg; 
% writetable(max_es_equity_weighted_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_equity_weighted_offset_costs_scc.csv");
% 
% max_es_equity_weighted_offset_env_outs = max_es_offset_urban_sprawl_equity_weighted_scc.env_outs;
% max_es_equity_weighted_offset_env_outs.new2kid = max_es_offset_urban_sprawl_equity_weighted_scc.new2kid;
% max_es_equity_weighted_offset_env_outs.hectares_chg = max_es_offset_urban_sprawl_equity_weighted_scc.hectares_chg; 
% writetable(max_es_equity_weighted_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_equity_weighted_offset_env_outs_scc.csv");
% 
% max_es_equity_weighted_offset_es_outs = max_es_offset_urban_sprawl_equity_weighted_scc.es_outs;
% max_es_equity_weighted_offset_es_outs.new2kid = max_es_offset_urban_sprawl_equity_weighted_scc.new2kid;
% max_es_equity_weighted_offset_es_outs.hectares_chg = max_es_offset_urban_sprawl_equity_weighted_scc.hectares_chg; 
% writetable(max_es_equity_weighted_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_es_equity_weighted_offset_es_outs_scc.csv");
% 
% % Max. recreation 
% % ---------------
% max_rec_offset_benefits = max_rec_offset_urban_sprawl_scc.benefits;
% max_rec_offset_benefits.new2kid = max_rec_offset_urban_sprawl_scc.new2kid;
% max_rec_offset_benefits.hectares_chg = max_rec_offset_urban_sprawl_scc.hectares_chg; 
% writetable(max_rec_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_benefits_scc.csv");
% 
% max_rec_offset_costs = max_rec_offset_urban_sprawl_scc.costs;
% max_rec_offset_costs.new2kid = max_rec_offset_urban_sprawl_scc.new2kid;
% max_rec_offset_costs.hectares_chg = max_rec_offset_urban_sprawl_scc.hectares_chg; 
% writetable(max_rec_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_costs_scc.csv");
% 
% max_rec_offset_env_outs = max_rec_offset_urban_sprawl_scc.env_outs;
% max_rec_offset_env_outs.new2kid = max_rec_offset_urban_sprawl_scc.new2kid;
% max_rec_offset_env_outs.hectares_chg = max_rec_offset_urban_sprawl_scc.hectares_chg; 
% writetable(max_rec_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_env_outs_scc.csv");
% 
% max_rec_offset_es_outs = max_rec_offset_urban_sprawl_scc.es_outs;
% max_rec_offset_es_outs.new2kid = max_rec_offset_urban_sprawl_scc.new2kid;
% max_rec_offset_es_outs.hectares_chg = max_rec_offset_urban_sprawl_scc.hectares_chg; 
% writetable(max_rec_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_offset_es_outs_scc.csv");
% 
% Max. recreation equity weighted
% -------------------------------
max_rec_equity_weighted_offset_benefits = max_equity_offset_scc.benefits;
max_rec_equity_weighted_offset_benefits.new2kid = max_equity_offset_scc.new2kid;
max_rec_equity_weighted_offset_benefits.hectares_chg = max_equity_offset_scc.hectares_chg; 
writetable(max_rec_equity_weighted_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_equity_weighted_offset_benefits_scc.csv");

max_rec_equity_weighted_offset_costs = max_equity_offset_scc.costs;
max_rec_equity_weighted_offset_costs.new2kid = max_equity_offset_scc.new2kid;
max_rec_equity_weighted_offset_costs.hectares_chg = max_equity_offset_scc.hectares_chg; 
writetable(max_rec_equity_weighted_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_equity_weighted_offset_costs_scc.csv");

max_rec_equity_weighted_offset_env_outs = max_equity_offset_scc.env_outs;
max_rec_equity_weighted_offset_env_outs.new2kid = max_equity_offset_scc.new2kid;
max_rec_equity_weighted_offset_env_outs.hectares_chg = max_equity_offset_scc.hectares_chg; 
writetable(max_rec_equity_weighted_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_equity_weighted_offset_env_outs_scc.csv");

max_rec_equity_weighted_offset_es_outs = max_equity_offset_scc.es_outs;
max_rec_equity_weighted_offset_es_outs.new2kid = max_equity_offset_scc.new2kid;
max_rec_equity_weighted_offset_es_outs.hectares_chg = max_equity_offset_scc.hectares_chg; 
writetable(max_rec_equity_weighted_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\max_rec_equity_weighted_offset_es_outs_scc.csv");

% Min cost
% --------
min_cost_offset_benefits = min_cost_offset_scc.benefits;
min_cost_offset_benefits.new2kid = min_cost_offset_scc.new2kid;
min_cost_offset_benefits.hectares_chg = min_cost_offset_scc.hectares_chg; 
writetable(min_cost_offset_benefits,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\min_cost_offset_benefits_scc.csv");

min_cost_offset_costs = min_cost_offset_scc.costs;
min_cost_offset_costs.new2kid = min_cost_offset_scc.new2kid;
min_cost_offset_costs.hectares_chg = min_cost_offset_scc.hectares_chg; 
writetable(min_cost_offset_costs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\min_cost_offset_costs_scc.csv");

min_cost_offset_env_outs = min_cost_offset_scc.env_outs;
min_cost_offset_env_outs.new2kid = min_cost_offset_scc.new2kid;
min_cost_offset_env_outs.hectares_chg = min_cost_offset_scc.hectares_chg; 
writetable(min_cost_offset_env_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\min_cost_offset_env_outs_scc.csv");

min_cost_offset_es_outs = min_cost_offset_scc.es_outs;
min_cost_offset_es_outs.new2kid = min_cost_offset_scc.new2kid;
min_cost_offset_es_outs.hectares_chg = min_cost_offset_scc.hectares_chg; 
writetable(min_cost_offset_es_outs,"D:\Documents\GitHub\BNG\Output\baseline_2031_urbanisation\Offset_outputs\min_cost_offset_es_outs_scc.csv");




