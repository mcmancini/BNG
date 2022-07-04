%% run_jncc_discrete.m
%  ===================
% Author: Mattia Mancini, Rebecca Collins
% Created: 01 Jul 2022
% Last modified: 01 Jul 2022
% ---------------------------------------
% DESCRIPTION
% Script to run the JNCC biodiversity model on a set of discrete site size
% changes, for all 2km cells in the UK. This is done to generate a lookup
% table matching species richness changes to hectares of land use change in
% each cell, which can be used to identify sites that provide the minimum
% legal requirement of 10% biodiversity uplift for each development
% project.
% =========================================================================

%% (0) Set up
%  ==========
clear
addpath(genpath('D:\Documents\GitHub\NEV\'));
data_dir       = 'D:\Documents\NEV\Model Data\';
lcm_data_folder = 'D:\Documents\GitHub\BNG\Data\LCM\LCM_2km\';
biodiversity_climate_string = 'future';
server_flag = false;
conn = fcn_connect_database(server_flag);
SetDataPaths;

%% (2) LAND USES
%  =============

% 2.1. Load land uses. 
% --------------------
lcm_data_path = 'D:\Documents\GitHub\BNG\Data\LCM\LCM_2km\';
baseline_lcm = readtable(strcat(lcm_data_path, 'lcm_aggr_2000.csv'));

urbanisation_data_path = 'D:\Documents\GitHub\BNG\Data\Urban Sprawl - F.Eigenbrod\';
urbanisation_lcm = readtable(strcat(landuse_data_path, 'urban_sprawl_2031_HDens.csv'));

%% (3) RUN AGRICULTURAL MODEL FOR LCM AND URBANISATION
%  ===================================================
parameters = fcn_set_parameters();
carbon_price = fcn_get_carbon_price(conn, parameters.carbon_price);
es_agriculture = fcn_run_agriculture(agriculture_data_folder, ...
                                     climate_data_folder, ...
                                     agricultureghg_data_folder, ...
                                     parameters, ...
                                     landuses, ...
                                     carbon_price);


