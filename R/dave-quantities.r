
# Daily gridcell-level outputs
add_pft_output("file_agb", "AGB", "Above-Ground Biomass", "kg/m2", "gridcell", "daily")
add_pft_output("file_agb_tree", "AGB", "Above-Ground Tree Biomass", "kg/m2", "gridcell", "daily")
add_pft_output("file_dlai", "LAI", "Leaf Area Index", "m2/m2", "gridcell", "daily")
add_output_static("file_dflux", "C Fluxes", "Carbon Fluxes", "", c(
    "Veg" = "kgC/m2/day",
    "Repr" = "kgC/m2/day",
    "Soil" = "kgC/m2/day",
    "Fire" = "kgC/m2/day",
    "Est" = "kgC/m2/day",
    "NEE" = "kgC/m2/day"
), "gridcell", "daily")
add_output_static("file_dcmass_grass", "Grass C Mass", "Grass Carbon Mass", "", c(
    "L0" = "kgC/m2/day",
    "L1" = "kgC/m2/day",
    "L2" = "kgC/m2/day",
    "L3" = "kgC/m2/day",
    "Ltot" = "kgC/m2/day",
    "R" = "kgC/m2/day",
    "LRtot" = "kgC/m2/day",
    "Cstore" = "kgC/m2/day",
    "NPP" = "kgC/m2/day",
    "g_fac" = "kgC/m2/day",
    "gpp_fac" = "kgC/m2/day",
    "sb_fac" = "kgC/m2/day",
    "wscal_fac" = "kgC/m2/day",
    "tscal_fac" = "kgC/m2/day",
    "s_fac" = "kgC/m2/day",
    "frost_sen" = "kgC/m2/day",
    "wscal_sen" = "kgC/m2/day",
    "tscal_sen" = "kgC/m2/day"
), "gridcell", "daily")

# Daily PFT-level outputs
add_pft_output("file_dave_lai", "LAI", "Leaf Area Index", "m2/m2", "patch", "daily")
add_pft_output("file_dave_fpc", "FPC", "Foliar Projective Cover", "", "patch", "daily")
add_pft_output("file_dave_crownarea", "Crown Area", "Crown Area", "m2", "patch", "daily")
add_pft_output("file_dave_agd_g", "Gross Photosynthesis", "Gross Photosynthesis", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_rd_g", "Leaf Respiration", "Leaf Respiration", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_je", "PAR-limited Photosynthesis", "PAR-limited photosynthetic rate", "gC/m2/h", "patch", "daily")
add_pft_output("file_dave_vm", "RuBisCO", "RuBisCO capacity", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_fphen_activity", "Phenology Activity", "Dormancy Downregulation", "0-1", "patch", "daily")
add_pft_output("file_dave_fdev_growth", "Development Factor", "Development Factor for growth demand", "0-1", "patch", "daily")
add_pft_output("file_dave_frepr_cstruct", "Reproductive Ratio", "Ratio of reproductive to aboveground structural biomass", "", "patch", "daily")
add_pft_output("file_dave_growth_demand", "Growth Demand", "Growth Demand", "0-1", "patch", "daily")
add_pft_output("file_dave_transpiration", "Transpiration", "Transpiration", "mm/day", "patch", "daily")
add_pft_output("file_dave_nscal", "N Stress", "Nitrogen Stress (1=no stress)", "0-1", "patch", "daily")
add_pft_output("file_dave_nscal_mean", "N Stress", "Nitrogen Stress (5 day running mean)", "0-1", "patch", "daily")
add_pft_output("file_dave_ltor", "Leaf:Root Ratio", "Leaf:Root Ratio", "", "patch", "daily")
add_pft_output("file_dave_cue", "CUE", "Carbon Use Efficiency", "", "patch", "daily")
add_pft_output("file_dave_alpha_leaf", "Leaf Alpha", "Leaf Sink Strength", "0-1", "patch", "daily")
add_pft_output("file_dave_alpha_root", "Root Alpha", "Root Sink Strength", "0-1", "patch", "daily")
add_pft_output("file_dave_alpha_sap", "Sap Alpha", "Sap Sink Strength", "0-1", "patch", "daily")
add_pft_output("file_dave_alpha_repr", "Reproductive Alpha", "Reproductive Sink Strength", "0-1", "patch", "daily")
add_pft_output("file_dave_cmass", "C Mass", "PFT-Level Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_leaf_limit", "Leaf Limit", "Optimum Leaf C Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_root_limit", "Root Limit", "Optimum Root C Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_sap_limit", "Sap Limit", "Optimum Sap C Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_repr_limit", "Reproductive Limit", "Optimum Reproductive C Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_storage_limit", "Storage Limit", "Optimum Storage C Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cgrow", "C Demand", "Total Carbon Demand", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cgrow_leaf", "Leaf C Demand", "Leaf Carbon Demand", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cgrow_root", "Root C Demand", "Root Carbon Demand", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cgrow_sap", "Sap C Demand", "Sap Carbon Demand", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cgrow_repr", "Reproductive C Demand", "Reproductive Carbon Demand", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_diameter_inc", "Diameter Increment", "Diameter Increment", "m/day", "patch", "daily")
add_pft_output("file_dave_height_inc", "Height Increment", "Height Increment", "m/day", "patch", "daily")
add_pft_output("file_dave_height", "Height", "Plant Height", "m", "patch", "daily")
add_pft_output("file_dave_diameter", "Diameter", "Stem Diameter", "m", "patch", "daily")
add_pft_output("file_dave_basalarea", "Basal Area", "Basal Area", "m2/tree", "patch", "daily")
add_pft_output("file_dave_basalarea_inc", "Basal Area Increment", "Basal Area Increment", "m2/tree/day", "patch", "daily")
add_pft_output("file_dave_dturnover_leaf", "Leaf Turnover", "Leaf C Turnover", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_dturnover_root", "Root Turnover", "Root C Turnover", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_dturnover_sap", "Sap Turnover", "Sapwood C Turnover", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_anc_frac", "ANC Fraction", "Fraction of Photosynthesis Limited by Rubisco", "0-1", "patch", "daily")
add_pft_output("file_dave_anj_frac", "ANJ Fraction", "Fraction of Photosynthesis Limited by RuBP Regeneration", "0-1", "patch", "daily")
add_pft_output("file_dave_anp_frac", "ANP Fraction", "Fraction of Photosynthesis Limited by TPU", "0-1", "patch", "daily")
add_pft_output("file_dave_dnuptake", "N Uptake", "Nitrogen Uptake", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_cexcess", "Carbon Overflow", "Carbon Overflow", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_ctolitter_leaf", "Leaf C to Litter", "Leaf Carbon to Litter", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_ntolitter_leaf", "Leaf N to Litter", "Leaf Nitrogen to Litter", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_ctolitter_root", "Root C to Litter", "Root Carbon to Litter", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_ntolitter_root", "Root N to Litter", "Root Nitrogen to Litter", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_ctolitter_repr", "Reproductive C to Litter", "Reproductive Carbon to Litter", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_ntolitter_repr", "Reproductive N to Litter", "Reproductive Nitrogen to Litter", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_ctolitter_crown", "Crown C to Litter", "Crown Carbon to Litter", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_ntolitter_crown", "Crown N to Litter", "Crown Nitrogen to Litter", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_aboveground_cmass", "Above-Ground C Mass", "Above-Ground Carbon Biomass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_belowground_cmass", "Below-Ground C Mass", "Below-Ground Carbon Biomass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_aboveground_nmass", "Above-Ground N Mass", "Above-Ground Nitrogen Biomass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_belowground_nmass", "Below-Ground N Mass", "Below-Ground Nitrogen Biomass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_aboveground_tree_biomass", "Above-Ground Tree Biomass", "Above-Ground Tree Biomass", "kg/m2", "patch", "daily")
add_pft_output("file_dave_live_biomass", "Live Biomass", "Live Biomass", "kg/m2", "patch", "daily")
add_pft_output("file_dave_indiv_npp", "NPP", "Net Primary Productivity", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_sla", "SLA", "Specific Leaf Area", "m2/kgC", "patch", "daily")
add_pft_output("file_dave_cmass_leaf", "Leaf C Mass", "Green Leaf Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_leaf_brown", "Brown Leaf C Mass", "Brown Leaf Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_leaf", "Leaf N Mass", "Green Leaf Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_cmass_crown", "Crown C Mass", "Crown Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_repr", "Reproductive C Mass", "Reproductive Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_root", "Root C Mass", "Root Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_root", "Root N Mass", "Root Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_nmass", "N Mass", "Vegetation N Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_cmass_storage", "Storage C Mass", "Non-Structural Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_cmass_storage_max", "Storage C Capacity", "Non-Structural Carbon Capacity", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_storage", "Storage N Mass", "Non-Structural Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_nmass_storage_max", "Max Storage N Mass", "Max Non-Structural Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_cmass_sap", "Sapwood C Mass", "Sapwood Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_sap", "Sapwood N Mass", "Sapwood Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_cmass_heart", "Heartwood C Mass", "Heartwood Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_heart", "Heartwood N Mass", "Heartwood Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_nmass_repr", "Reproductive N Mass", "Reproductive Nitrogen Mass", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_ndemand", "N Demand", "Nitrogen Demand", "kgN/m2/day", "patch", "daily")
add_pft_output("file_dave_density", "Density", "Tree Density", "/m2", "patch", "daily")
add_pft_output("file_dave_sapwood_area", "Sapwood Area", "Sapwood Area", "m2", "patch", "daily")
add_pft_output("file_dave_latosa", "LA:SA", "Leaf Area to Sapwood Area Ratio", "", "patch", "daily")
add_pft_output("file_dave_fpar", "FPAR", "Fraction of Absorbed PAR", "0-1", "patch", "daily")
add_pft_output("file_dave_indiv_gpp", "GPP", "Gross Primary Productivity", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_resp_autotrophic", "Autotrophic Respiration", "Autotrophic Respiration", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_resp_maintenance", "Maintenance Respiration", "Maintenance Respiration", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_resp_growth", "Growth Respiration", "Growth Respiration", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_layerwise_fpar", "FPAR", "Layerwise Fraction of Absorbed PAR", "0-1", "patch", "daily")
add_pft_output("file_dave_layerwise_lai", "LAI", "Layerwise Leaf Area Index", "m2/m2", "patch", "daily")
add_pft_output("file_dave_wscal", "Water Stress", "Water Stress (1=no stress)", "0-1", "patch", "daily")
add_pft_output("file_dave_cmass_litter_repr", "Reproductive C Litter", "Reproductive Carbon in Litter", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_nmass_litter_repr", "Reproductive N Litter", "Reproductive Nitrogen in Litter", "kgN/m2", "patch", "daily")
add_pft_output("file_dave_dresp", "Autotrophic Respiration", "Autotrophic Respiration by PFT", "kgC/m2/day", "patch", "daily")
add_pft_output("file_dave_cmass_seed_ext", "Grass Seedbank", "Grass Seedbank Carbon Mass", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_subdaily_an", "Net Photosynthesis", "Net Photosynthesis", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_rd", "Leaf Respiration", "Leaf Respiration", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_anc", "RuBisCO-limited Photosynthesis", "RuBisCO-limited Photosynthesis", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_anj", "RuBP-Limited Photosynthesis", "RuBP Regeneration-Limited Photosynthesis", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_gsw", "Stomatal Conductance", "Stomatal conductance to Water Vapour", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_ci", "CO2 Concentration", "Intercellular CO2 Concentration", "mol/mol", "patch", "subdaily")
add_pft_output("file_dave_subdaily_vcmax", "Vcmax", "Maximum Carboxylation Rate", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_subdaily_jmax", "Jmax", "Maximum Electron Transport Rate", "mol/m2/s", "patch", "subdaily")
add_pft_output("file_dave_sw", "Soil Water", "Soil Water Fraction Full", "mm", "patch", "daily")
add_pft_output("file_dave_swmm", "Soil Water", "Soil Water Content", "mm", "patch", "daily")
add_pft_output("file_dave_swvol", "Soil Water", "Volumetric Soil Water Content", "m3/m3", "patch", "daily")
add_pft_output("file_dave_cfluxes_patch", "Patch C Fluxes", "Carbon fluxes", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_cfluxes_pft", "PFT C Fluxes", "Daily PFT-level carbon fluxes", "gC/m2/day", "patch", "daily")
add_pft_output("file_dave_anetps_ff_max", "Max Forest Floor Net Photosynthesis", "Maximum Recorded Annual Net Forest-Floor Photosynthesis", "kgC/m2", "patch", "daily")
add_pft_output("file_dave_absolute_alpha_leaf", "Absolute Alpha Leaf", "Absolute Alpha Leaf", "", "patch", "daily")
add_pft_output("file_dave_absolute_alpha_root", "Absolute Alpha Root", "Absolute Alpha Root", "", "patch", "daily")
add_pft_output("file_dave_absolute_alpha_sap", "Absolute Alpha Sap", "Absolute Alpha Sap", "", "patch", "daily")
add_pft_output("file_dave_absolute_alpha_storage", "Absolute Alpha Storage", "Absolute Alpha Storage", "", "patch", "daily")
add_pft_output("file_dave_alpha_storage", "Alpha Storage", "Alpha Storage", "", "patch", "daily")
add_pft_output("file_dave_cdeficit_leaf", "Cdeficit Leaf", "Cdeficit Leaf", "", "patch", "daily")
add_pft_output("file_dave_cdeficit_root", "Cdeficit Root", "Cdeficit Root", "", "patch", "daily")
add_pft_output("file_dave_cdeficit_sap", "Cdeficit Sap", "Cdeficit Sap", "", "patch", "daily")
add_pft_output("file_dave_cdeficit_storage", "Cdeficit Storage", "Cdeficit Storage", "", "patch", "daily")
add_pft_output("file_dave_cgrazed_brown_leaf", "Cgrazed Brown Leaf", "Cgrazed Brown Leaf", "", "patch", "daily")
add_pft_output("file_dave_cgrazed_green_leaf", "Cgrazed Green Leaf", "Cgrazed Green Leaf", "", "patch", "daily")
add_pft_output("file_dave_cgrazed_repr", "Cgrazed Repr", "Cgrazed Repr", "", "patch", "daily")
add_pft_output("file_dave_cgrow_pot", "Cgrow Pot", "Cgrow Pot", "", "patch", "daily")
add_pft_output("file_dave_cgrow_storage", "Cgrow Storage", "Cgrow Storage", "", "patch", "daily")
add_pft_output("file_dave_clitter", "Clitter", "Clitter", "", "patch", "daily")
add_pft_output("file_dave_cmass_germ", "Cmass Germ", "Cmass Germ", "", "patch", "daily")
add_pft_output("file_dave_cmass_standing_heart", "Cmass Standing Heart", "Cmass Standing Heart", "", "patch", "daily")
add_pft_output("file_dave_cmass_standing_leaf", "Cmass Standing Leaf", "Cmass Standing Leaf", "", "patch", "daily")
add_pft_output("file_dave_cmass_standing_repr", "Cmass Standing Repr", "Cmass Standing Repr", "", "patch", "daily")
add_pft_output("file_dave_cmass_standing_root", "Cmass Standing Root", "Cmass Standing Root", "", "patch", "daily")
add_pft_output("file_dave_cmass_standing_sap", "Cmass Standing Sap", "Cmass Standing Sap", "", "patch", "daily")
add_pft_output("file_dave_cmass_storage_dynam", "Cmass Storage Dynam", "Cmass Storage Dynam", "", "patch", "daily")
add_pft_output("file_dave_cpool", "Cpool", "Cpool", "", "patch", "daily")
add_pft_output("file_dave_creturn", "Creturn", "Creturn", "", "patch", "daily")
add_pft_output("file_dave_csenesc", "Csenesc", "Csenesc", "", "patch", "daily")
add_pft_output("file_dave_cton_crown", "Cton Crown", "Cton Crown", "", "patch", "daily")
add_pft_output("file_dave_cton_heart", "Cton Heart", "Cton Heart", "", "patch", "daily")
add_pft_output("file_dave_cton_leaf", "Cton Leaf", "Cton Leaf", "", "patch", "daily")
add_pft_output("file_dave_cton_root", "Cton Root", "Cton Root", "", "patch", "daily")
add_pft_output("file_dave_cton_sap", "Cton Sap", "Cton Sap", "", "patch", "daily")
add_pft_output("file_dave_cton_storage", "Cton Storage", "Cton Storage", "", "patch", "daily")
add_pft_output("file_dave_cturnover_crown", "Cturnover Crown", "Cturnover Crown", "", "patch", "daily")
add_pft_output("file_dave_cturnover_leaf", "Cturnover Leaf", "Cturnover Leaf", "", "patch", "daily")
add_pft_output("file_dave_cturnover_repr", "Cturnover Repr", "Cturnover Repr", "", "patch", "daily")
add_pft_output("file_dave_cturnover_root", "Cturnover Root", "Cturnover Root", "", "patch", "daily")
add_pft_output("file_dave_cturnover_sap", "Cturnover Sap", "Cturnover Sap", "", "patch", "daily")
add_pft_output("file_dave_dncohort", "Dncohort", "Dncohort", "", "patch", "daily")
add_pft_output("file_dave_dnpool", "Dnpool", "Dnpool", "", "patch", "daily")
add_pft_output("file_dave_dphen", "Dphen", "Dphen", "", "patch", "daily")
add_pft_output("file_dave_dresponse_window", "Dresponse Window", "Dresponse Window", "", "patch", "daily")
add_pft_output("file_dave_fevap", "Fevap", "Fevap", "", "patch", "daily")
add_pft_output("file_dave_fnuptake", "Fnuptake", "Fnuptake", "", "patch", "daily")
add_pft_output("file_dave_fphoto", "Fphoto", "Fphoto", "", "patch", "daily")
add_pft_output("file_dave_frepr_cstorage", "Frepr Cstorage", "Frepr Cstorage", "", "patch", "daily")
add_pft_output("file_dave_fstorage", "Fstorage", "Fstorage", "", "patch", "daily")
add_pft_output("file_dave_ftemp", "Ftemp", "Ftemp", "", "patch", "daily")
add_pft_output("file_dave_fwater", "Fwater", "Fwater", "", "patch", "daily")
add_pft_output("file_dave_g1", "G1", "G1", "", "patch", "daily")
add_pft_output("file_dave_jmax25", "Jmax25", "Jmax25", "", "patch", "daily")
add_pft_output("file_dave_kmax_drought_affected", "Kmax Drought Affected", "Kmax Drought Affected", "", "patch", "daily")
add_pft_output("file_dave_ksoil_layer", "Ksoil Layer", "Ksoil Layer", "", "patch", "daily")
add_pft_output("file_dave_lai_today", "Lai Today", "Lai Today", "", "patch", "daily")
add_pft_output("file_dave_layerwise_an", "Layerwise An", "Layerwise An", "", "patch", "daily")
add_pft_output("file_dave_max_k_loss", "Max K Loss", "Max K Loss", "", "patch", "daily")
add_pft_output("file_dave_meanleafage", "Meanleafage", "Meanleafage", "", "patch", "daily")
add_pft_output("file_dave_meanrootage", "Meanrootage", "Meanrootage", "", "patch", "daily")
add_pft_output("file_dave_meansapage", "Meansapage", "Meansapage", "", "patch", "daily")
add_pft_output("file_dave_met_par_max", "Met Par Max", "Met Par Max", "", "patch", "daily")
add_pft_output("file_dave_ngrazed_brown_leaf", "Ngrazed Brown Leaf", "Ngrazed Brown Leaf", "", "patch", "daily")
add_pft_output("file_dave_ngrazed_green_leaf", "Ngrazed Green Leaf", "Ngrazed Green Leaf", "", "patch", "daily")
add_pft_output("file_dave_ngrazed_repr", "Ngrazed Repr", "Ngrazed Repr", "", "patch", "daily")
add_pft_output("file_dave_nitrogen_budget_annual", "Nitrogen Budget Annual", "Nitrogen Budget Annual", "", "patch", "annual")
add_pft_output("file_dave_nitrogen_budget_daily", "Nitrogen Budget Daily", "Nitrogen Budget Daily", "", "patch", "daily")
add_pft_output("file_dave_nlitter", "Nlitter", "Nlitter", "", "patch", "daily")
add_pft_output("file_dave_nmass_crown", "Nmass Crown", "Nmass Crown", "", "patch", "daily")
add_pft_output("file_dave_nmass_standing_heart", "Nmass Standing Heart", "Nmass Standing Heart", "", "patch", "daily")
add_pft_output("file_dave_nmass_standing_leaf", "Nmass Standing Leaf", "Nmass Standing Leaf", "", "patch", "daily")
add_pft_output("file_dave_nmass_standing_repr", "Nmass Standing Repr", "Nmass Standing Repr", "", "patch", "daily")
add_pft_output("file_dave_nmass_standing_root", "Nmass Standing Root", "Nmass Standing Root", "", "patch", "daily")
add_pft_output("file_dave_nmass_standing_sap", "Nmass Standing Sap", "Nmass Standing Sap", "", "patch", "daily")
add_pft_output("file_dave_nretranslocated", "Nretranslocated", "Nretranslocated", "", "patch", "daily")
add_pft_output("file_dave_nstorage_drawdown", "Nstorage Drawdown", "Nstorage Drawdown", "", "patch", "daily")
add_pft_output("file_dave_nturnover_crown", "Nturnover Crown", "Nturnover Crown", "", "patch", "daily")
add_pft_output("file_dave_nturnover_leaf", "Nturnover Leaf", "Nturnover Leaf", "", "patch", "daily")
add_pft_output("file_dave_nturnover_repr", "Nturnover Repr", "Nturnover Repr", "", "patch", "daily")
add_pft_output("file_dave_nturnover_root", "Nturnover Root", "Nturnover Root", "", "patch", "daily")
add_pft_output("file_dave_nturnover_sap", "Nturnover Sap", "Nturnover Sap", "", "patch", "daily")
add_pft_output("file_dave_par_grass", "Par Grass", "Par Grass", "", "patch", "annual")
add_pft_output("file_dave_patch_ba", "Patch Ba", "Patch Ba", "", "patch", "daily")
add_pft_output("file_dave_pet", "Pet", "Pet", "", "patch", "daily")
add_pft_output("file_dave_phen", "Phen", "Phen", "", "patch", "daily")
add_pft_output("file_dave_rel_par", "Rel Par", "Rel Par", "", "patch", "daily")
add_pft_output("file_dave_resp_crown", "Resp Crown", "Resp Crown", "", "patch", "daily")
add_pft_output("file_dave_resp_leaf", "Resp Leaf", "Resp Leaf", "", "patch", "daily")
add_pft_output("file_dave_resp_repr", "Resp Repr", "Resp Repr", "", "patch", "daily")
add_pft_output("file_dave_resp_root", "Resp Root", "Resp Root", "", "patch", "daily")
add_pft_output("file_dave_resp_sap", "Resp Sap", "Resp Sap", "", "patch", "daily")
add_pft_output("file_dave_root_beta", "Root Beta", "Root Beta", "", "patch", "daily")
add_pft_output("file_dave_rpc", "Rpc", "Rpc", "", "patch", "daily")
add_pft_output("file_dave_Rs_r", "Rs R", "Rs R", "", "patch", "daily")
add_pft_output("file_dave_soil_nh4_mass", "Soil Nh4 Mass", "Soil Nh4 Mass", "", "patch", "daily")
add_pft_output("file_dave_soil_no3_mass", "Soil No3 Mass", "Soil No3 Mass", "", "patch", "daily")
add_pft_output("file_dave_soil_psi", "Soil Psi", "Soil Psi", "", "patch", "daily")
add_pft_output("file_dave_soil_psi_layer", "Soil Psi Layer", "Soil Psi Layer", "", "patch", "daily")
add_pft_output("file_dave_soiltemp25", "Soiltemp25", "Soiltemp25", "", "patch", "daily")
add_pft_output("file_dave_sompool_decay_reduction", "Sompool Decay Reduction", "Sompool Decay Reduction", "", "patch", "daily")
add_pft_output("file_dave_sompool_frac_remaining", "Sompool Frac Remaining", "Sompool Frac Remaining", "", "patch", "daily")
add_pft_output("file_dave_sompool_nimmob", "Sompool Nimmob", "Sompool Nimmob", "", "patch", "daily")
add_pft_output("file_dave_sompool_nmin_gross", "Sompool Nmin Gross", "Sompool Nmin Gross", "", "patch", "daily")
add_pft_output("file_dave_sompool_ntoc", "Sompool Ntoc", "Sompool Ntoc", "", "patch", "daily")
add_pft_output("file_dave_spft_cmass_repr", "Spft Cmass Repr", "Spft Cmass Repr", "", "patch", "daily")
add_pft_output("file_dave_standing_dead", "Standing Dead", "Standing Dead", "", "patch", "daily")
add_pft_output("file_dave_storage_dynam_14sum", "Storage Dynam 14sum", "Storage Dynam 14sum", "", "patch", "daily")
add_pft_output("file_dave_subdaily_lwp", "Subdaily Lwp", "Subdaily Lwp", "", "patch", "daily")
add_pft_output("file_dave_subdaily_Vl", "Subdaily Vl", "Subdaily Vl", "", "patch", "daily")
add_pft_output("file_dave_swavail_100", "Swavail 100", "Swavail 100", "", "patch", "daily")
add_pft_output("file_dave_swmm_10", "Swmm 10", "Swmm 10", "", "patch", "daily")
add_pft_output("file_dave_swmm_100", "Swmm 100", "Swmm 100", "", "patch", "daily")
add_pft_output("file_dave_swmm_600", "Swmm 600", "Swmm 600", "", "patch", "daily")
add_pft_output("file_dave_turnover_leaf", "Turnover Leaf", "Turnover Leaf", "", "patch", "daily")
add_pft_output("file_dave_vcmax", "Vcmax", "Vcmax", "", "patch", "daily")
add_pft_output("file_dave_vcmax25", "Vcmax25", "Vcmax25", "", "patch", "daily")
add_pft_output("file_dave_water_budget_annual", "Water Budget Annual", "Water Budget Annual", "", "patch", "annual")
add_pft_output("file_dave_water_budget_daily", "Water Budget Daily", "Water Budget Daily", "", "patch", "daily")
add_pft_output("file_dave_water_uptake", "Water Uptake", "Water Uptake", "", "patch", "daily")
add_pft_output("file_dave_wcont_upper", "Wcont Upper", "Wcont Upper", "", "patch", "daily")
add_pft_output("file_dave_weighted_psi", "Weighted Psi", "Weighted Psi", "", "patch", "daily")


# These aren't really PFT outputs, but they have dynamic column names
# (one column per timestep). Therefore, for now I'm treating them as
# PFT outputs.
add_pft_output("file_dave_met_subdaily_temp", "Temperature", "Air Temperature", "°C", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_par", "PAR", "Photosynthetically Active Radiation", "kJ/m2/timestep", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_vpd", "VPD", "Vapor Pressure Deficit", "kPa", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_insol", "Insolation", "Insolation (units depend on instype)", "kJ/m2/timestep", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_precip", "Precipitation", "Precipitation", "mm", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_pressure", "Pressure", "Atmospheric Pressure", "kPa", "patch", "subdaily")
add_pft_output("file_dave_met_subdaily_co2", "CO2", "Atmospheric CO2 Concentration", "ppm", "patch", "subdaily")
add_output_static("file_dave_met_pressure", "Pressure", "Atmospheric pressure", "kPa", c("pressure"), "patch", "daily")
add_output_static("file_dave_met_co2", "CO2", "Atmospheric CO2 concentration", "ppm", c("co2"), "patch", "daily")
add_output_static("file_dave_met_temp", "Temperature", "Air Temperature", "°C", c("temp"), "patch", "daily")
add_output_static("file_dave_met_par", "PAR", "Photosynthetically Active Radiation", "kJ/m2/timestep", c("par"), "patch", "daily")
add_output_static("file_dave_met_vpd", "VPD", "Vapor Pressure Deficit", "kPa", c("vpd"), "patch", "daily")
add_output_static("file_dave_met_insol", "Insolation", "Insolation", "", c("insol"), "patch", "daily")
add_output_static("file_dave_met_precip", "Precipitation", "precipitation", "mm", c("precip"), "patch", "daily")

# Annual individual-level outputs.
add_output_static("file_dave_indiv_age", "Age", "Plant Age", "Years", c("age"), "individual", "annual")
add_output_static("file_dave_indiv_c_d_dbh", "DBH", "Change in DBH", "m", c("d_dbh"), "individual", "annual")
add_output_static("file_dave_indiv_mort", "Mortality", "Mortality Fractions", "0-1", c("mort_age", "mort_greff"), "individual", "annual")

# Daily individual-level outputs.
add_output_static("file_dave_indiv_cpool", "C Pools", "Vegetation Carbon Pools", "kgC/m2", c("cmass_leaf", "cmass_root", "cmass_crown", "cmass_sap", "cmass_heart", "cmass_repr", "cmass_storage"), "individual", "daily")
add_output_static("file_dave_indiv_npool", "N Pools", "Vegetation Nitrogen Pools", "kgN/m2", c("nmass_leaf", "nmass_root", "nmass_crown", "nmass_sap", "nmass_heart", "nmass_repr", "nmass_storage"), "individual", "daily")
add_output_static("file_dave_indiv_lai", "LAI", "Leaf Area Index", "m2/m2", c("lai"), "individual", "daily")
add_output_static("file_dave_hydraulics", "Hydraulics", "Hydraulics Diagnostics", "", c("kmax", "lai", "minimum_lwp"), "individual", "daily")
add_output_static("file_dave_indiv_alpha_leaf", "Alpha Leaf", "Leaf Sink Strength", "0-1", c("alpha_leaf"), "individual", "daily")
add_output_static("file_dave_indiv_alpha_root", "Alpha Root", "Root Sink Strength", "0-1", c("alpha_root"), "individual", "daily")
add_output_static("file_dave_indiv_alpha_sap", "Alpha Sap", "Sapwood Sink Strength", "0-1", c("alpha_sap"), "individual", "daily")
add_output_static("file_dave_indiv_alpha_storage", "Alpha Storage", "Storage Sink Strength", "0-1", c("alpha_storage"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow", "Total C Demand", "Total C Demand", "kgC/m2", c("cgrow"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow_leaf", "Leaf C Demand", "Leaf C Demand", "kgC/m2", c("cgrow_leaf"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow_repr", "Reproductive C Demand", "Reproductive C Demand", "kgC/m2", c("cgrow_repr"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow_root", "Root C Demand", "Root C Demand", "kgC/m2", c("cgrow_root"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow_sap", "Sapwood C Demand", "Sapwood C Demand", "kgC/m2", c("cgrow_sap"), "individual", "daily")
add_output_static("file_dave_indiv_cgrow_storage", "Storage C Demand", "Storage C Demand", "kgC/m2", c("cgrow_storage"), "individual", "daily")
add_output_static("file_dave_indiv_crownarea", "Crown Area", "Crown Area", "m2", c("crownarea"), "individual", "daily")
add_output_static("file_dave_indiv_dbh", "DBH", "Diameter at Bole Height", "m", c("diam", "deltadiam"), "individual", "daily")
add_output_static("file_dave_indiv_density", "Density", "Tree Density", "/m2", c("density"), "individual", "daily")
add_output_static("file_dave_indiv_fpar", "FPAR", "Fraction of PAR Absorbed by Foliage", "-", c("fpar"), "individual", "daily")
add_output_static("file_dave_indiv_fpc", "FPC", "Foliar Projective Cover", "-", c("fpc"), "individual", "daily")
add_output_static("file_dave_indiv_height", "Height", "Plant Height", "m", c("height"), "individual", "daily")
add_output_static("file_dave_indiv_litter_pools", "Litter Pools", "Litter Pools", "kgC/m2", c("Leaf", "Root", "Sap", "Heart", "Repr"), "individual", "daily")
add_output_static("file_dave_indiv_respiration", "Respiration", "Indiv Respiration", "kgC/m2", c("resp_leaf", "resp_root", "resp_sap", "resp_crown", "resp_repr", "resp_maintenance", "resp_growth", "total"), "individual", "daily")
add_output_static("file_dave_indiv_tree_cpool", "Tree Carbon Pools", "Indiv Tree Cpool", "kgC/m2", c(
    "cmass_leaf",
    "cmass_root",
    "cmass_sap",
    "cmass_heart",
    "cmass_repr",
    "cmass_storage",
    "cmass_leaf_limit",
    "cmass_root_limit",
    "cmass_sap_limit",
    "cmass_storage_limit"), "individual", "daily")

# Annual patch-level outputs
add_output_static("file_dave_patch_age", "Patch Age", "Time Since Disturbance", "years", c("age"), "patch", "annual")
add_output_static("file_dave_arunoff", "Runoff", "Runoff", "mm", c(
    "Surf",
    "Drain",
    "Base",
    "Precip",
    "NetInf"
), "patch", "annual")
add_output_static("file_dave_globfirm", "Globfirm", "GLOBFIRM Outputs", "", c(
    "fireprob"
), "patch", "annual")

add_output_static("file_dave_acpool", "C Pools", "Carbon Pools", "kgC/m2", c(
    "VegC",
    "StandingC",
    # ifcentury
    "LittC",
    "SoilfC",
    "SoilsC",
    # !ifcentury
    "LitterC",
    "SoilC",
    # run_landcover && ifslowharvestpool
    "HarvSlowC",
    "total"
), "patch", "annual")

add_output_static("file_dave_anpool", "N Pools", "Nitrogen Pools", "kgN/m2", c(
    "VegN",
    "StandingN",
    "LitterN",
    "SoilN",
    "AvailN"
), "patch", "annual")

add_output_static("file_dave_acflux", "C Fluxes", "Carbon Fluxes", "kgC/m2", c(
    "Veg",
    "Repr",
    "Soil",
    "Fire",
    "Est",
    # if (run_landcover)
    "Seed",
    "Harvest",
    "LU_ch",
    "Slow_h",
    "NEE" # always
), "patch", "annual")

add_monthly_output("file_dave_mwcont_upper", "Water Content", "Water Content Fraction of Upper Soil Layer", "0-1", "patch")
add_monthly_output("file_dave_mwcont_lower", "Water Content", "Water Content Fraction of Lower Soil Layer", "0-1", "patch")
add_output_static("file_dave_apet", "PET", "Potential Evapotranspiration", "mm", c("pet"), "patch", "annual")
add_output_static("file_dave_asimfire", "Simfire", "Simfire analysis", "", c(
    # ("burned_area", "fraction"),
    # ("fire_carbon", "gC/m2")
    Biome = "-",
    MxNest = "-",
    CurrentNesterov = "-",
    PopDens = "inhabitants/km2",
    AMxFApar = "0-1",
    FireProb = "0-1",
    Region = "-",
    PBiome = "-",
    PFpar = "0-1",
    PMxNest = "-",
    PPopDens = "inhabitants/km2",
    MFireChance = "0-1",
    DFireChance = "0-1",
    avail_fuel = "g/m2",
    SimfireBurntArea = "0-1",
    ABurntArea = "sum(0-1)"
), "patch", "annual")

add_output_static("file_dave_afuel", "Fuel", "Blaze fuel availability", "gC/m2", c("FineLitter", "FineWood", "CoarseWood", "GrassCMass", "avail_fuel"), "patch", "annual")
add_output_static("file_dave_acoarse_woody_debris", "CWD", "Coarse Woody Debris", "gC/m2", c("LitterHeart", "som", "k_tun_litter"), "patch", "annual")
add_output_static("file_dave_amet_year", "Met Year", "Current year of met data being used", "year", c("met_year"), "patch", "annual")
add_output_static("file_dave_aco2", "CO2", "Atmospheric CO2 Concentration", "ppm", c("co2"), "patch", "annual")
add_output_static("file_dave_aminleach", "Mineral N Leaching", "Leaching of Soil Mineral N", "kgN/m2/yr", c("aminleach"), "patch", "annual")
add_output_static("file_dave_sompool_acmass", "SOM Pool C Mass", "Surface Organic Matter Carbon Mass by Pool", "kgC/m2", c(
    "SURFSTRUCT",
    "SOILSTRUCT",
    "SOILMICRO",
    "SURFHUMUS",
    "SURFMICRO",
    "SURFMETA",
    "SURFFWD",
    "SURFCWD",
    "SOILMETA",
    "SLOWSOM",
    "PASSIVESOM",
    "total"
), "patch", "annual")

add_output_static("file_dave_sompool_anmass", "SOM Pool N Mass", "Surface Organic Matter Nitrogen Mass by Pool", "kgN/m2", c(
    "SURFSTRUCT",
    "SOILSTRUCT",
    "SOILMICRO",
    "SURFHUMUS",
    "SURFMICRO",
    "SURFMETA",
    "SURFFWD",
    "SURFCWD",
    "SOILMETA",
    "SLOWSOM",
    "PASSIVESOM",
    "total"
), "patch", "annual")

add_output_static("file_dave_andep", "N Deposition", "Nitrogen Deposition", "kgN/m2", c(
    "dNO3dep",
    "dNH4dep",
    "nfert",
    "total"
), "patch", "annual")

add_output_static("file_dave_anfixation", "N Fixation", "Nitrogen Fixation", "kgN/m2", c(
    "nfixation"
), "patch", "annual")

# Daily patch-level outputs
add_output_static("file_dave_daylength", "Day Length", "Day Length", "h", c("daylength"), "patch", "daily")
add_output_static("file_dave_soil_nmass_avail", "Available Soil N", "Soil N Mass Available for Plant Uptake", "kgN/m2", c("nmass"), "patch", "daily")
add_output_static("file_dave_dsimfire", "Simfire", "Simfire Analysis", "", c(
    "Biome" = "0=NOVEG, 1=CROP, 2=NEEDLELEAF, 3=BROADLEAF, 4=MIXED_FOREST, 5=SHRUBS, 6=SAVANNA, 7=TUNDRA, 8=BARREN",
    "MxNest" = "-",
    "CurrentNesterov" = "-",
    "PopDens" = "inhabitants/km2",
    "AMxFApar" = "0-1",
    "FireProb" = "0-1",
    "Region" = "-",
    "PBiome" = "-",
    "PFpar" = "0-1",
    "PMxNest" = "-",
    "PPopDens" = "inhabitants/km2",
    "MFireChance" = "0-1",
    "DFireChance" = "0-1",
    "avail_fuel" = "g/m2",
    "SimfireBurntArea" = "0-1",
    "ABurntArea" = "sum(0-1)"), "patch", "daily")

add_output_static("file_dave_sompool_cmass", "SOM Pool C Mass", "Surface Organic Matter Carbon Mass by Pool", "kgC/m2", c("cmass"), "patch", "daily")
add_output_static("file_dave_sompool_nmass", "SOM Pool N Mass", "Surface Organic Matter Nitrogen Mass by Pool", "kgN/m2", c("nmass"), "patch", "daily")
add_output_static("file_dave_ninput", "N Input", "Nitrogen Deposition", "kgN/m2", c("dNO3dep", "dNH4dep", "nfert", "total"), "patch", "daily")
add_output_static("file_dave_fpar_ff", "Forest-Floor FPAR", "Fraction of Photosynthetically Active Radiation Reaching the Forest Floor", "0-1", c("fpar_ff"), "patch", "daily")
add_output_static("file_dave_aet", "ET", "Evaporation and Transpiration", "mm", c("evap", "transp", "total"), "patch", "daily")
add_output_static("file_dave_resp_heterotrophic", "Heterotrophic Respiration", "Heterotrophic respiration", "gC/m2/day", c("resp_heterotrophic"), "patch", "daily")
add_output_static("file_dave_resp", "Respiration", "Ecosystem Respiration", "gC/m2/day", c("resp"), "patch", "daily")
add_output_static("file_dave_gpp", "GPP", "Gross Primary Productivity", "gC/m2/day", c("gpp"), "patch", "daily")
add_output_static("file_dave_npp", "NPP", "Net Primary Productivity", "gC/m2/day", c("npp"), "patch", "daily")
add_output_static("file_dave_nee", "NEE", "Net Ecosystem Exchange", "gC/m2/day", c("nee"), "patch", "daily")
add_output_static("file_dave_evaporation", "Evaporation", "Evaporation", "mm/day", c("evaporation", "interception", "transpiration", "evapotranspiration"), "patch", "daily")
add_output_static("file_dave_soilc", "Soil Carbon", "Soil Carbon Carbon Mass", "kgC/m2", c("surfsoillitterc", "cwdc", "centuryc", "total"), "patch", "daily")
add_output_static("file_dave_soiln", "Soil Nitrogen", "Soil Nitrogen Nitrogen Mass", "kgN/m2", c("surfsoillittern", "cwdn", "centuryn", "mineraln", "total"), "patch", "daily")
add_output_static("file_dave_soil_nflux", "Soil N Flux", "Soil Nitrogen Flux", "kgN/m2/day", c("nflux"), "patch", "daily")
add_output_static("file_dave_dfuel", "Fuel Availability", "Fuel Availability for Blaze", "kgC/m2", c("FineLitter", "FineWood", "CoarseWood", "GrassCMass", "avail_fuel"), "patch", "daily")
add_output_static("file_dave_dcoarse_woody_debris", "CWD", "Coarse Woody Debris Carbon Mass", "gC/m2", c("LitterHeart", "som", "k_tun_litter"), "patch", "daily")

# Annual patch-level PFT outputs.
add_pft_output("file_dave_alai", "LAI", "Leaf Area Index", "m2/m2", "patch", "annual")
add_pft_output("file_dave_afpc", "FPC", "Foliage Projective Cover", "0-1", "patch", "annual")
add_pft_output("file_dave_acmass", "C Mass", "Carbon Mass", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_anmass", "N Mass", "Nitrogen Mass", "kgN/m2", "patch", "annual")
add_pft_output("file_dave_aheight", "Height", "Plant Height", "m", "patch", "annual")
add_pft_output("file_dave_aaet", "AET", "Actual Evapotranspiration", "mm", "patch", "annual")
add_pft_output("file_dave_adensity", "Density", "Density of individuals over patch", "/m2", "patch", "annual")
add_pft_output("file_dave_altor", "Leaf:Root Ratio", "Leaf:Root Ratio", "", "patch", "annual")
add_pft_output("file_dave_anuptake", "N Uptake", "Vegetation Nitrogen Uptake", "kgN/m2/year", "patch", "annual")
add_pft_output("file_dave_a_aboveground_cmass", "Above-Ground C Mass", "Above-Ground Carbon biomass", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_a_belowground_cmass", "Below-Ground C Mass", "Below-Ground Carbon biomass", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_a_aboveground_nmass", "Above-Ground N Mass", "Above-Ground Nitrogen biomass", "kgN/m2", "patch", "annual")
add_pft_output("file_dave_a_belowground_nmass", "Belowground N Mass", "Belowground Nitrogen biomass", "kgN/m2", "patch", "annual")
add_pft_output("file_dave_anpp", "NPP", "Net Primary Productivity", "kgC/m2/year", "patch", "annual")
add_pft_output("file_dave_agpp", "GPP", "Gross Primary Productivity", "kgC/m2/year", "patch", "annual")
add_pft_output("file_dave_aresp", "Respiration", "Autotrophic Respiration", "kgC/m2/year", "patch", "annual")
add_pft_output("file_dave_acmass_mort", "Mortality C Mass", "Carbon Mass of Killed Vegetation", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_aclitter", "C Litter", "Carbon Litter", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_ancohort", "Number of Cohorts", "Number of Cohorts of each PFT", "count", "patch", "annual")
add_pft_output("file_dave_anetps_ff", "Forest-Floor Net Photosynthesis", "Net photosynthesis at forest floor", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_leaf", "Leaf C Allocation", "Carbon Allocation to Leaf", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_root", "Root C Allocation", "Carbon Allocation to Root", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_repr", "Repr C Allocation", "Carbon Allocation to Repr", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_sap", "Sapwood C Allocation", "Carbon Allocation to Sapwood", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_crown", "Crown C Allocation", "Carbon Allocation to Crown", "kgC/m2", "patch", "annual")
add_pft_output("file_dave_acalloc_storage", "Acalloc Storage", "Acalloc Storage", "", "patch", "annual")
add_pft_output("file_dave_acmass_germ", "Acmass Germ", "Acmass Germ", "", "patch", "annual")
add_pft_output("file_dave_acue", "Acue", "Acue", "", "patch", "annual")
add_pft_output("file_dave_amort_age", "Amort Age", "Amort Age", "", "patch", "annual")
add_pft_output("file_dave_amort_greff", "Amort Greff", "Amort Greff", "", "patch", "annual")
add_pft_output("file_dave_anee", "Anee", "Anee", "", "patch", "annual")
add_pft_output("file_dave_anflux", "Anflux", "Anflux", "", "patch", "annual")
add_pft_output("file_dave_anretranslocated", "Anretranslocated", "Anretranslocated", "", "patch", "annual")
add_pft_output("file_dave_ansapling", "Ansapling", "Ansapling", "", "patch", "annual")
add_pft_output("file_dave_ansapling_year", "Ansapling Year", "Ansapling Year", "", "patch", "annual")
add_pft_output("file_dave_anstorage_drawdown", "Anstorage Drawdown", "Anstorage Drawdown", "", "patch", "annual")
add_pft_output("file_dave_aresponse_window", "Aresponse Window", "Aresponse Window", "", "patch", "annual")
add_pft_output("file_dave_cton_leaf_max", "Cton Leaf Max", "Cton Leaf Max", "", "patch", "annual")
add_pft_output("file_dave_cton_leaf_min", "Cton Leaf Min", "Cton Leaf Min", "", "patch", "annual")
add_pft_output("file_dave_cton_root_max", "Cton Root Max", "Cton Root Max", "", "patch", "annual")
add_pft_output("file_dave_cton_root_min", "Cton Root Min", "Cton Root Min", "", "patch", "annual")
add_pft_output("file_dave_cton_sap_max", "Cton Sap Max", "Cton Sap Max", "", "patch", "annual")
add_pft_output("file_dave_cton_sap_min", "Cton Sap Min", "Cton Sap Min", "", "patch", "annual")


# Annual stand-level outputs
add_output_static("file_dave_stand_frac", "Stand Fraction", "Fraction of the gridcell occupied by each stand", "", c("fraction"), "stand", "annual")
add_output_static("file_dave_stand_type", "Stand Type", "Stand landcover types", "", c("type"), "stand", "annual")

# Annual gridcell-level PFT outputs.
add_pft_output("file_cmass", "C Mass", "Total Carbon Biomass", "kgC/m2", "gridcell", "annual")
add_pft_output("file_anpp", "NPP", "Net Primary Production", "kgC/m2/year", "gridcell", "annual")
add_pft_output("file_agpp", "GPP", "Gross Primary Production", "kgC/m2/year", "gridcell", "annual")
add_pft_output("file_fpc", "FPC", "Foliage Projective Cover", "0-1", "gridcell", "annual")
add_pft_output("file_aaet", "AET", "Actual Evapotranspiration", "mm/year", "gridcell", "annual")
add_pft_output("file_lai", "LAI", "Leaf Area Index", "m2/m2", "gridcell", "annual")
add_pft_output("file_aagb", "AGB", "Above-Ground Biomass", "kg/m2", "gridcell", "annual")

# Annual gridcell-level outputs.
add_output_static("file_dave_amet_prec", "Precipitation", "Precipitation", "mm/year", c("prec"), "gridcell", "annual")
add_output_static("file_dave_amet_par", "PAR", "Photosynthetically Active Radiation", "J/m2/year", c("par"), "gridcell", "annual")
add_output_static("file_cflux", "Carbon Fluxes", "Carbon Fluxes", "kgC/m2/year", c(
    "Veg",
    "Repr",
    "Soil",
    "Fire",
    "Est",
    "Seed",
    "Harvest",
    "LU_ch",
    "Slow_h",
    "NEE"
), "gridcell", "annual")

add_pft_output("file_doc", "Dissolved Organic Carbon", "Dissolved Organic Carbon", "kgC/m2", "gridcell", "annual")
add_pft_output("file_dens", "Tree Density", "Tree Density", "indiv/m2", "gridcell", "annual")

add_output_static("file_cpool", "Soil Carbon", "Soil Carbon Pools", "kgC/m2", c(
    "VegC",
    "LitterC",
    "SoilC",
    "Total"
), "gridcell", "annual")

add_pft_output("file_clitter", "Carbon Litter", "Carbon in litter", "kgC/m2", "gridcell", "annual")
add_output_static("file_runoff", "Runoff", "Runoff", "mm/year", c(
    "Surf",
    "Drain",
    "Base",
    "Total"), "gridcell", "annual")

add_output_static("file_wetland_water_added", "Wetland Water Added", "Water Added to Wetland", "mm", c("H2OAdded"), "gridcell", "annual")
add_pft_output("file_speciesheights", "Species Height", "Mean Species Height", "m", "gridcell", "annual")
add_pft_output("file_speciesdiam", "Species Diameter", "Mean Species Diameter", "m", "gridcell", "annual")
add_output_static("file_firert", "Fire Return Time", "Fire Return Time", "", c(
    FireRT = "years",
    BurntFr = "0-1"), "gridcell", "annual")

add_pft_output("file_nmass", "N Mass", "Nitrogen Content in Vegetation", "kgN/m2", "gridcell", "annual")
add_pft_output("file_cton_leaf", "Leaf C:N Ratio", "Carbon to Nitrogen Ratio in Leaves", "-", "gridcell", "annual")
add_output_static("file_nsources", "N Sources", "Nitrogen Sources", "kgN/m2/year", c(
    "NH4dep",
    "NO3dep",
    "fix",
    "fert",
    "input",
    "min",
    "imm",
    "netmin",
    "Total"
), "gridcell", "annual")

add_output_static("file_npool", "N Pools", "Nitrogen Pools", "kgN/m2", c(
    "VegN",
    "LitterN",
    "SoilN",
    "Total"
), "gridcell", "annual")

add_pft_output("file_nlitter", "N Litter", "Litter Nitrogen Mass", "kgN/m2", "gridcell", "annual")
add_pft_output("file_nuptake", "N Uptake", "Nitrogen Uptake", "kgN/m2/year", "gridcell", "annual")
add_pft_output("file_vmaxnlim", "Vmax N Limitation", "Nitrogen Limitation on Vmax", "", "gridcell", "annual")

add_output_static("file_nflux", "N Fluxes", "Nitrogen Fluxes", "kgN/m2/year", c(
    "NH4dep",
    "NO3dep",
    "fix",
    "fert",
    "est",
    "flux",
    "leach",
    "NEE",
    "Total"
), "gridcell", "annual")

add_output_static("file_ngases", "N Gas Emissions", "Nitrogen Gas Emissions", "kgN/m2/year", c(
    "NH3_fire",
    "NH3_soil",
    "NOx_fire",
    "NOx_soil",
    "N2O_fire",
    "N2O_soil",
    "N2_fire",
    "N2_soil",
    "Total"
), "gridcell", "annual")

add_pft_output("file_aiso", "Isoprene Flux", "Isoprene Flux", "mgC/m2/year", "gridcell", "annual")
add_pft_output("file_amon", "Monoterpene Flux", "Monoterpene Flux", "mgC/m2/year", "gridcell", "annual")
add_pft_output("file_amon_mt1", "Endocyclic Monoterpene Flux", "Endocyclic Monoterpene Flux", "mgC/m2/year", "gridcell", "annual")
add_pft_output("file_amon_mt2", "Other Monoterpene Flux", "Other Monoterpene Flux", "mgC/m2/year", "gridcell", "annual")
add_output_static("file_aburned_area_out", "BLAZE Burned Area", "BLAZE Burned Area", "", c("BurntFr" = "0-1"), "gridcell", "annual")
add_output_static("file_simfireanalysis_out", "SIMFIRE Analytics", "SIMFIRE Analytics", "", c(
    "Biome" = "0=NOVEG, 1=CROP, 2=NEEDLELEAF, 3=BROADLEAF, 4=MIXED_FOREST, 5=SHRUBS, 6=SAVANNA, 7=TUNDRA, 8=BARREN",
    "MxNest" = "-",
    "PopDens" = "inhabitants/km2",
    "AMxFApar" = "0-1",
    "FireProb" = "0-1",
    "Region" = "unused"), "gridcell", "annual")

# Monthly gridcell-level outputs
add_monthly_output("file_mnpp", "NPP", "Net Primary Productivity", "kgC/m2/month", "gridcell")
add_monthly_output("file_mlai", "LAI", "Leaf Area Index", "m2/m2", "gridcell")
add_monthly_output("file_mgpp", "GPP", "Gross Primary Productivity", "kgC/m2/month", "gridcell")
add_monthly_output("file_mra", "Ra", "Autotrophic Respiration", "kgC/m2/month", "gridcell")
add_monthly_output("file_maet", "AET", "Actual Evapotranspiration", "mm/month", "gridcell")
add_monthly_output("file_mpet", "PET", "Potential Evapotranspiration", "mm/month", "gridcell")
add_monthly_output("file_mevap", "Evaporation", "Evaporation", "mm/month", "gridcell")
add_monthly_output("file_mrunoff", "Runoff", "Runoff", "mm/month", "gridcell")
add_monthly_output("file_mintercep", "Interception", "Interception", "mm/month", "gridcell")
add_monthly_output("file_mrh", "Rh", "Heterotrophic Respiration", "kgC/m2/month", "gridcell")
add_monthly_output("file_mnee", "NEE", "Net Ecosystem Exchange", "kgC/m2/month", "gridcell")
add_monthly_output("file_mwcont_upper", "Water Content", "Water Content Fraction of Upper Soil Layer", "", "gridcell")
add_monthly_output("file_mwcont_lower", "Water Content", "Water Content Fraction of Lower Soil Layer", "", "gridcell")
add_monthly_output("file_miso", "Isoprene Flux", "Isoprene Flux", "mgC/m2/month", "gridcell")
add_monthly_output("file_mmon", "Monoterpene Flux", "Monoterpene Flux", "mgC/m2/month", "gridcell")
add_monthly_output("file_mmon_mt1", "Endocyclic Monoterpene Flux", "Endocyclic Monoterpene Flux", "mgC/m2", "gridcell")
add_monthly_output("file_mmon_mt2", "Other Monoterpene Flux", "Other Monoterpene Flux", "mgC/m2", "gridcell")
add_monthly_output("file_msoiltempdepth5", "Soil Temperature", "Soil temperature (5cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth15", "Soil Temperature", "Soil temperature (15cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth25", "Soil Temperature", "Soil temperature (25cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth35", "Soil Temperature", "Soil temperature (35cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth45", "Soil Temperature", "Soil temperature (45cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth55", "Soil Temperature", "Soil temperature (55cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth65", "Soil Temperature", "Soil temperature (65cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth75", "Soil Temperature", "Soil temperature (75cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth85", "Soil Temperature", "Soil temperature (85cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth95", "Soil Temperature", "Soil temperature (95cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth105", "Soil Temperature", "Soil temperature (105cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth115", "Soil Temperature", "Soil temperature (115cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth125", "Soil Temperature", "Soil temperature (125cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth135", "Soil Temperature", "Soil temperature (135cm)", "degC", "gridcell")
add_monthly_output("file_msoiltempdepth145", "Soil Temperature", "Soil temperature (145cm)", "degC", "gridcell")
add_monthly_output("file_mch4", "CH4 Emissions", "CH4 emissions (Total)", "kgC/m2/year", "gridcell")
add_monthly_output("file_mch4diff", "CH4 Emissions", "CH4 emissions (Diffusion)", "kgC/m2/year", "gridcell")
add_monthly_output("file_mch4plan", "CH4 Emissions", "CH4 emissions (Plant-mediated)", "kgC/m2/year", "gridcell")
add_monthly_output("file_mch4ebull", "CH4 Emissions", "CH4 emissions (Ebullition)", "kgC/m2/year", "gridcell")
add_monthly_output("file_msnow", "Snow Depth", "Snow depth", "m", "gridcell")
add_monthly_output("file_mwtp", "Water Table Depth", "Water table depth", "m", "gridcell")
add_output_static("file_mald", "Active Layer Depth", "Active layer depth",
    "m", c(get_global("MODEL_MONTH_COLS"), "MAXALD"), "gridcell", "monthly")
add_monthly_output("file_mburned_area_out", "BLAZE Burned Area", "BLAZE Burned Area", "0-1", "gridcell")
