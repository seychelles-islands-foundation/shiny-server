all: prepare

pd = ../data/processed/
rd = ../data/raw/

prepare: $(pd)tide_pred/loc_1/2016.rds $(pd)tide_pred/loc_1/2017.rds

$(pd)tide_pred/loc_1/2016.rds $(pd)tide_pred/loc_1/2017.rds: 03_pregenerate_tidal_predictions.R $(pd)tide_models.rds
	Rscript $<

$(pd)tide_models.rds: 02_tidal-models.R $(pd)depth_time-series.rds
	Rscript $<

$(pd)depth_time-series.rds: 01_read-data.R $(rd)pressure_data/*
	Rscript $<