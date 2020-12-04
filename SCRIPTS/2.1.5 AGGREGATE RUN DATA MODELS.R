

# data for modelling ####

model_data<-run_data%>%
    mutate(media_label = ifelse(media_label == "Sand 0.5-1mm & Anthracite 0.8-1.6mm","SA","FL"),
           upwash_label = gsub(" upwash","",upwash_label),
           blend_fct = gsub(" Blend","", blend_fct))

## OLS models ####

lm_v<- function(DF){
    mdl <- lm(value ~ upwash_rate+media_label, data = DF)
    return(mdl)
}

lm_vmf<- function(DF){
    mdl <- lm(value  ~ rel_to_vmf+media_label, data = DF)
    return(mdl)
}


lm_v_only<- function(DF){
    mdl <- lm(value~upwash_rate, data = DF)
    return(mdl)
}


## OLS LOWEST LEVEL FITTED ####


linear_models_lowest_level<-model_data%>%
    ungroup()%>%
    dplyr::select(blend_fct, media_label, grp,run_id, censored, upwash_rate,rel_to_vmf,temp, brkthru, NORM_CBHL,vol_adj_norm_hl,upwash_empty_bed_vols)%>%
    rename(NCBHL = NORM_CBHL,
           VNHL =vol_adj_norm_hl)%>%
    pivot_longer(cols = c(brkthru, NCBHL,VNHL))%>%
    mutate(censored_brkthru = ifelse(name =="brkthru" & censored ==T, "CB","OK"))%>%
    dplyr::filter(censored_brkthru =="OK")%>%
    select(-censored_brkthru)%>%
    group_by( name, blend_fct, media_label)%>%
    nest()%>%
    mutate(lm_v_only = map(.x = data, .f = lm_v_only))


linear_models_lowest_level%>%
    dplyr::filter(name == "brkthru" )%>%
    arrange(blend_fct)%>%
    stargazer(.$lm_v_only, ci=T,
              type = "html", out = "TABLES/OLS LOW LEV WR BRKTHRU.html",
              column.labels = paste(.$blend_fct, .$media_label),
              dep.var.caption = "Particle breakthrough",
              covariate.labels = c("Backwash rate (m/h)"),
              digits = 1)

linear_models_lowest_level%>%
    dplyr::filter(name == "NCBHL" )%>%
    arrange(blend_fct)%>%
    stargazer(.$lm_v_only, ci=T,
              type = "html", out = "TABLES/OLS LOW LEV WR NCBHL.html",
              column.labels = paste(.$blend_fct, .$media_label),
              dep.var.caption = "Normalised clean bed head loss (NCBHL)",
              covariate.labels = c("Backwash rate (m/h)"),
              digits = 1)


linear_models_lowest_level%>%
    dplyr::filter(name == "VNHL" )%>%
    arrange(blend_fct)%>%
    stargazer(.$lm_v_only, ci=T,
              type = "html", out = "TABLES/OLS LOW LEV WR VNHL.html",
              column.labels = paste(.$blend_fct, .$media_label),
              dep.var.caption = "Volume normalised head loss (VNHL)",
              covariate.labels = c("Backwash rate (m/h)"),
              digits = 1)



# OLS MEDIA ONLY MODELS ### 

lm_med<- function(DF){
    mdl <- lm(value ~ media_label, data = DF)
    return(mdl)
}

lmer_med<- function(DF){
    mdl <- lmer(value ~ media_label  +
                    (1|upwash_rate), data = DF)
    return(mdl)
}


media_only_models<-model_data%>%
    ungroup()%>%
    dplyr::select(blend_fct, media_label, grp,run_id, censored, upwash_rate,rel_to_vmf,temp, brkthru, NORM_CBHL,vol_adj_norm_hl, upwash_empty_bed_vols)%>%
    pivot_longer(cols = c(brkthru, NORM_CBHL,vol_adj_norm_hl))%>%
    mutate(censored_brkthru = ifelse(name =="brkthru" & censored ==T, "CB","OK"))%>%
    dplyr::filter(censored_brkthru =="OK")%>%
    select(-censored_brkthru)%>%
    group_by( name, blend_fct)%>%
    nest()%>%
    mutate(lm_med= map(.x = data, .f = lm_med),
           lmer_med= map(.x = data, .f = lmer_med)
    )%>%
    pivot_longer(cols = lm_med:lmer_med, names_to = "model_type", values_to = "model" )%>%
    mutate(loglik = map(.x = model, .f = logLik),
           AIC = map_dbl(.x = model, .f = AIC))

media_only_models%>%
    group_by(name, blend_fct)%>% #,FE,mdl_typ
    dplyr::filter(model_type == "lmer_med")%>%
    mutate(name = gsub("NORM_CBHL" , "NCBHL (m)",name),
           name = gsub("vol_adj_norm_hl" , "VNHL (mm/EBV)",name),
           name = gsub("brkthru" , "BRKTHU (EBV)",name))%>%
    arrange(name)%>%
    stargazer(.$model, ci=T,
              type = "html", out = "TABLES/LME MEDIA ONLY MODELS.html",
              model.names = T,
              model.type = T,
              column.labels = paste(.$blend_fct,.$name),
              covariate.labels = c("Media = SA"),
              #dep.var.caption = "Particle breakthrough",
              digits = 1)


# REML models ####


lm_reml_v<- function(DF){
    mdl <- gls(value ~ upwash_rate+media_label, data = DF, method = "REML")
    return(mdl)
}

lm_reml_vmf<- function(DF){
    mdl <- gls(value  ~ rel_to_vmf+media_label, data = DF, method = "REML")
    return(mdl)
}


lm_reml_v_int<- function(DF){
    mdl <- gls(value ~ upwash_rate*media_label, data = DF, method = "REML")
    return(mdl)
}

lm_reml_vmf_int<- function(DF){
    mdl <- gls(value  ~ rel_to_vmf*media_label, data = DF, method = "REML")
    return(mdl)
}

lmer_reml_v_rnd_int<- function(DF){
    mdl <- lmer(value ~ upwash_rate+media_label  +
                    (1|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_vmf_rnd_int<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf+media_label  +
                    (1|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_v_rnd_intslp<- function(DF){
    mdl <- lmer(value ~ upwash_rate+media_label  +
                    (upwash_rate|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_vmf_rnd_intslp<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf+media_label  +
                    (rel_to_vmf|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}


lmer_reml_v_int_rnd_int<- function(DF){
    mdl <- lmer(value ~ upwash_rate*media_label  +
                    (1|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_vmf_int_rnd_int<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf*media_label  +
                    (1|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_v_int_rnd_intslp<- function(DF){
    mdl <- lmer(value ~ upwash_rate*media_label  +
                    (upwash_rate|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}

lmer_reml_vmf_int_rnd_intslp<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf*media_label  +
                    (rel_to_vmf|blend_fct), data = DF, REML = TRUE)
    return(mdl)
}



lmer_reml_v_all<- function(DF){
    mdl <- lmer(value~media_label+upwash_rate  +
                    (upwash_rate|blend_fct)+(1|run_id), data = DF, REML = TRUE)
}


lmer_reml_vmf_all<- function(DF){
    mdl <- lmer(value~media_label+rel_to_vmf  +
                    (rel_to_vmf|blend_fct)+(1|run_id), data = DF, REML = TRUE)
}



lmer_reml_v_int_all<- function(DF){
    mdl <- lmer(value~media_label*upwash_rate  +
                    (upwash_rate|blend_fct)+(1|run_id), data = DF, REML = TRUE)
}


lmer_reml_vmf_int_all<- function(DF){
    mdl <- lmer(value~media_label*rel_to_vmf  +
                    (rel_to_vmf|blend_fct)+(1|run_id), data = DF, REML = TRUE)
}



# ML models ####


lm_ml_v<- function(DF){
    mdl <- gls(value ~ upwash_rate+media_label, data = DF, method = "ML")
    return(mdl)
}

lm_ml_vmf<- function(DF){
    mdl <- gls(value  ~ rel_to_vmf+media_label, data = DF, method = "ML")
    return(mdl)
}


lm_ml_v_int<- function(DF){
    mdl <- gls(value ~ upwash_rate*media_label, data = DF, method = "ML")
    return(mdl)
}

lm_ml_vmf_int<- function(DF){
    mdl <- gls(value  ~ rel_to_vmf*media_label, data = DF, method = "ML")
    return(mdl)
}

lmer_ml_v_rnd_int<- function(DF){
    mdl <- lmer(value ~ upwash_rate+media_label  +
                    (1|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_vmf_rnd_int<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf+media_label  +
                    (1|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_v_rnd_intslp<- function(DF){
    mdl <- lmer(value ~ upwash_rate+media_label  +
                    (upwash_rate|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_vmf_rnd_intslp<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf+media_label  +
                    (rel_to_vmf|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}


lmer_ml_v_int_rnd_int<- function(DF){
    mdl <- lmer(value ~ upwash_rate*media_label  +
                    (1|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_vmf_int_rnd_int<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf*media_label  +
                    (1|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_v_int_rnd_intslp<- function(DF){
    mdl <- lmer(value ~ upwash_rate*media_label  +
                    (upwash_rate|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}

lmer_ml_vmf_int_rnd_intslp<- function(DF){
    mdl <- lmer(value  ~ rel_to_vmf*media_label  +
                    (rel_to_vmf|blend_fct), data = DF, REML = FALSE)
    return(mdl)
}



lmer_ml_v_all<- function(DF){
    mdl <- lmer(value~media_label+upwash_rate  +
                    (upwash_rate|blend_fct)+(1|run_id), data = DF, REML = FALSE)
}


lmer_ml_vmf_all<- function(DF){
    mdl <- lmer(value~media_label+rel_to_vmf  +
                    (rel_to_vmf|blend_fct)+(1|run_id), data = DF, REML = FALSE)
}




lmer_ml_v_int_all<- function(DF){
    mdl <- lmer(value~media_label*upwash_rate  +
                    (upwash_rate|blend_fct)+(1|run_id), data = DF, REML = FALSE)
}


lmer_ml_vmf_int_all<- function(DF){
    mdl <- lmer(value~media_label*rel_to_vmf  +
                    (rel_to_vmf|blend_fct)+(1|run_id), data = DF, REML = FALSE)
}



## mixed models all ####

run_mixed_models<-model_data%>%
    ungroup()%>%
    dplyr::select(blend_fct, media_label, grp,run_id, censored, upwash_rate,rel_to_vmf,temp, brkthru, NORM_CBHL,vol_adj_norm_hl, upwash_empty_bed_vols)%>%
    pivot_longer(cols = c(brkthru, NORM_CBHL,vol_adj_norm_hl))%>%
    mutate(censored_brkthru = ifelse(name =="brkthru" & censored ==T, "CB","OK"),
           summer_brkthru = ifelse(name =="brkthru" & blend_fct =="Summer", "SB","OK"))%>%
    dplyr::filter(censored_brkthru =="OK",
                  #summer_brkthru == "OK",
                  is.na(value)==F,
                  is.infinite(value)==F)%>%
    select(-censored_brkthru,-summer_brkthru)%>%
    group_by( name)%>%
    nest()%>%
    mutate(
        #lm_reml_v_mdl = map(.x = data, .f = lm_reml_v),
        #lm_reml_v_mdl_int = map(.x = data, .f = lm_reml_v_int),
        lmer_reml_v_rnd_int = map(.x = data, .f = lmer_reml_v_rnd_int),
        lmer_reml_v_int_rnd_int = map(.x = data, .f = lmer_reml_v_int_rnd_int),
        lmer_reml_v_rnd_intslp = map(.x = data, .f = lmer_reml_v_rnd_intslp),
        lmer_reml_v_int_rnd_intslp = map(.x = data, .f = lmer_reml_v_int_rnd_intslp),
        #lm_reml_vmf_mdl = map(.x = data, .f = lm_reml_vmf),
        #lm_reml_vmf_mdl_int = map(.x = data, .f = lm_reml_vmf_int),
        lmer_reml_vmf_rnd_int = map(.x = data, .f = lmer_reml_vmf_rnd_int),
        lmer_reml_vmf_int_rnd_int = map(.x = data, .f = lmer_reml_vmf_int_rnd_int),
        lmer_reml_vmf_rnd_intslp = map(.x = data, .f = lmer_reml_vmf_rnd_intslp),
        lmer_reml_vmf_int_rnd_intslp = map(.x = data, .f = lmer_reml_vmf_int_rnd_intslp),
        #lm_ml_v_mdl = map(.x = data, .f = lm_ml_v),
        #lm_ml_v_mdl_int = map(.x = data, .f = lm_ml_v_int),
        lmer_ml_v_rnd_int = map(.x = data, .f = lmer_ml_v_rnd_int),
        lmer_ml_v_int_rnd_int = map(.x = data, .f = lmer_ml_v_int_rnd_int),
        lmer_ml_v_rnd_intslp = map(.x = data, .f = lmer_ml_v_rnd_intslp),
        lmer_ml_v_int_rnd_intslp = map(.x = data, .f = lmer_ml_v_int_rnd_intslp),
        #lm_ml_vmf_mdl = map(.x = data, .f = lm_ml_vmf),
        #lm_ml_vmf_mdl_int = map(.x = data, .f = lm_ml_vmf_int),
        lmer_ml_vmf_rnd_int = map(.x = data, .f = lmer_ml_vmf_rnd_int),
        lmer_ml_vmf_int_rnd_int = map(.x = data, .f = lmer_ml_vmf_int_rnd_int),
        lmer_ml_vmf_rnd_intslp = map(.x = data, .f = lmer_ml_vmf_rnd_intslp),
        lmer_ml_vmf_int_rnd_intslp = map(.x = data, .f = lmer_ml_vmf_int_rnd_intslp)
    )%>%
    pivot_longer(cols = lmer_reml_v_rnd_int:lmer_ml_vmf_int_rnd_intslp, names_to = "model_type", values_to = "model" )%>%
    mutate(loglik = map(.x = model, .f = logLik),
           AIC = map_dbl(.x = model, .f = AIC))


mdl_labels<-data.frame(model_type = unique(run_mixed_models$model_type),
                       model_label = unique(run_mixed_models$model_type))%>%
    mutate(upw = ifelse(grepl("vmf",model_label)==T,"VMF","V"),
           mdl_typ = ifelse(grepl("lmer",model_label)==T,"LME","GLS"),
           method = ifelse(grepl("_reml_", model_label)==T, "REML","ML"),
           RE = gsub("_reml_","_",model_label),
           RE = gsub("_ml_","_",RE),
           RE = gsub("_"," ",RE),
           RE = gsub("mdl "," ",RE),
           RE = gsub("rnd intslp","RI&S",RE),
           RE = gsub("rnd int","RI",RE),
           RE = gsub("lmer ","",RE),
           RE = gsub("lm ","",RE),
           FE = ifelse(grepl("int",RE), "BW+MEDIA+BW:MEDIA","BW+MEDIA"),
           RE = gsub("int","",RE),
           RE = gsub("vmf ","",RE),
           RE = gsub("v ","",RE),
           RE = gsub("mdl","",RE),
           RE = gsub(" ","",RE))


run_mixed_models<-run_mixed_models%>%
    left_join(.,mdl_labels)


RES<-run_mixed_models%>%
    dplyr::filter(FE == "BW+MEDIA+BW:MEDIA",
                  method == "REML")%>%
    group_by(name, upw)%>%
    dplyr::filter(AIC == min(AIC))%>%
    dplyr::select(name,upw,RE)%>%
    ungroup()


colnames(SIG_FE_INT)

SIG_FE_INT<-inner_join(RES, run_mixed_models%>%dplyr::filter(method == "ML"))%>%
    select(name, upw,RE, FE, model)%>%
    pivot_wider(names_from = FE, values_from = model)%>%
    mutate(anova = map2(.x = `BW+MEDIA`, .y = `BW+MEDIA+BW:MEDIA`, .f = anova),
           tidied = map(.x = anova, .f = tidy))%>%
    select(name, upw,RE,  tidied)%>%
    unnest(tidied)%>%
    dplyr::filter(is.na(p.value)==F)%>%
    mutate(SIG_INT = ifelse(p.value < 0.05, 1,0),
           SIG_INT = ifelse(is.na(SIG_INT)==T,0,SIG_INT))%>%
    ungroup()%>%
    select(name,  upw, RE,SIG_INT)




SELECTED_MODELS<- inner_join(RES, run_mixed_models%>%dplyr::filter(method == "REML"))%>%
    left_join(., SIG_FE_INT)%>%
        mutate(KEEP = ifelse(SIG_INT==0 & FE == "BW+MEDIA+BW:MEDIA" ,0,1),
               KEEP = ifelse(SIG_INT==1 & FE == "BW+MEDIA" ,0,KEEP))%>%
    group_by(name, upw, RE)%>%
    dplyr::filter(KEEP == 1)


colnames(SELECTED_MODELS)
RESIDUALS<- SELECTED_MODELS%>%
    mutate(resid = map(.x = model, .f = resid))%>%
    select(model_label,name,  upw,mdl_typ, RE,FE, data, resid)%>%
    unnest(data,resid)
    

ggplot(RESIDUALS,aes(x = upwash_rate, y = resid, colour = media_label) )+
    geom_point()+
    facet_wrap(~model_label+name, scales = "free")+
    geom_smooth()

unique(SELECTED_MODELS$name)

SELECTED_MODELS%>%
    mutate(col_labs = gsub("brkthru", "BT (Hrs)", name),
           col_labs = gsub("NORM_CBHL", "NCBHL (m)", col_labs),
           col_labs = gsub("vol_adj_norm_hl", "VNHL (mm/bv)", col_labs))%>%
    stargazer(.$model, ci=T,
              type = "html", out = "TABLES/SELECTED.html",
              model.names = T,
              column.labels = paste(.$col_labs,.$RE),
             # covariate.labels = c("BW rate (m3/m2/hr)", "BW rate/ Vmf", "Media = Sand Anthracite",
              #                     "BW rate/ Vmf: Media = SA","BW rate: Media = SA"),
              #dep.var.caption = "Particle breakthrough",
              digits = 1,
              keep.stat = c("n","adj.rsq" ,"ll" ,"aic" ))



# Model winter breakthrough #####


lm_winter_brkthru_v<-lm(brkthru~ media_label+upwash_rate, 
                     data = model_data%>%
                         mutate(censored_brkthru = ifelse(censored ==T, "CB","OK"))%>%
                         dplyr::filter(censored_brkthru =="OK",
                                       blend_fct == "Winter"))


lm_winter_brkthru_vmf<-lm(brkthru~ media_label+rel_to_vmf, 
                     data = model_data%>%
                         mutate(censored_brkthru = ifelse(censored ==T, "CB","OK"))%>%
                         dplyr::filter(censored_brkthru =="OK",
                                       blend_fct == "Winter"))




stargazer(list(lm_winter_brkthru_v,lm_winter_brkthru_vmf), ci=T,
              type = "html", out = "TABLES/WINTER BRKTHU MDLS.html",
              model.names = T,
              #column.labels = paste(.$blend_fct,.$RE),
              covariate.labels = c( "Media = SA","BW rate (m3/m2/hr)", "BW rate/ Vmf"),
              dep.var.caption = "Particle breakthrough",
                dep.var.labels.include = F,
              digits = 1)






