%% CALC_SCENARIO_SUMMARIES
%  =======================
%  Authors: Mattia Mancini, Rebecca Collins
%  Created: 12-Jul-2022
%  Last modified: 12-Jul-2022
%  ----------------------------------------
%  DESCRIPTION
%  Script that takes the output of the runs of various BNG offset scenarios
%  and summarises into data that can be mapped and plotted, and from which
%  summary tables can be produced.
%  It requires matlab structured created running the script run_NEV.m and
%  saved in Section 4.
%  ========================================================================

%% (1) LOAD THE REQUIRED DATA
%  ==========================
clear
load('Output/local_bio_offset_urban_sprawl.mat')
load('Output/max_bio_offset_urban_sprawl_2031.mat')
load('Output/max_es_offset_urban_sprawl.mat')
load('Output/max_es_offset_urban_sprawl_equity_weighted.mat')

%% (2) SUMMARISE BIODIVERSITY CHANGES
%  ==================================


