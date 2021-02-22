

# Fundamental filtration theory functions

# Page 765 of MWH book


# relative size group (11-49)

f_Nr<- function(particle_diameter_m, collector_diameter_m){
    Nr = particle_diameter_m/collector_diameter_m
    return(Nr)
}


# test f_Nr value = 2.5e-4

f_Nr(particle_diameter_m = 1e-7, collector_diameter_m = 4e-4)


# gravity number dimensionless(11-50)

f_Ng<- function(particle_density_kgm3, liq_density_kgm3, particle_diameter_m, visc_kgms, filtration_rate_ms){
    Ng = (9.81*(particle_density_kgm3 - liq_density_kgm3)* particle_diameter_m^2)/(18*visc_kgms*filtration_rate_ms)
    return(Ng)
}

# test f_Ng 6.76e-8

f_Ng(particle_density_kgm3 = 1050, liq_density_kgm3 = 998, particle_diameter_m = 1e-7, visc_kgms = 1E-3, filtration_rate_ms = 15/3600)

# Peclet number dimensionless(11-40)

f_Pe<- function(visc_kgms, particle_diameter_m, collector_diameter_m,filtration_rate_ms, temp_absolute){
    Pe = (3*pi*visc_kgms*particle_diameter_m*collector_diameter_m*filtration_rate_ms)/(1.381e-23* temp_absolute)
    return(Pe)
}

# Test Pe 3.89e5

f_Pe(visc_kgms = 1e-3, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, filtration_rate_ms = 15/3600, temp_absolute = 293.15)

# attraction number dimensionless(11-51)

f_Na<- function(visc_kgms, particle_diameter_m, filtration_rate_ms){
    Na = 10e-20/(3*pi*visc_kgms*particle_diameter_m^2*filtration_rate_ms)
    return(Na)
}

# Test na 2.54e-2

f_Na(visc_kgms = 1e-3, particle_diameter_m = 1e-7, filtration_rate_ms = 15/3600)



# van der walls number dimensionless (11-52)

f_Nvdw<- function(temp_absolute){
    Nvdw = 1e-20/(1.381e-23*temp_absolute)
    return(Nvdw)
}

# Test vdw 2.47

f_Nvdw(293.15)

# porosity coefficient dimensionless (11-53)

f_por_coef<- function(porosity){
    por_coef = (1-porosity)^(1/3)
    return(por_coef)
}


# Test por coef 0.7937

f_por_coef(0.5)

# porosity function (11-54)

f_As <- function(porosity){
    por_coef = f_por_coef(porosity)
    
    As = (2*(1- por_coef^5))/(2- 3* por_coef + 3 * por_coef^5 - 2 * por_coef^6 )
    
    return(As)
}


# Test f_As 21.46

f_As(porosity = 0.5)


# absolute temp function

f_temp_abs<- function(temp_degc){
    temp_absolute = temp_degc+273.15
    return(temp_absolute)
}


hyd_lkup<-read.csv("DATA/hydraulics_lkup.csv")%>%transmute(temp = Temp,
                                                           Density = Density*1000,
                                                           DynViscosity = DynViscosity/1000)

# yao interception

f_yao_interception<- function(particle_diameter_m, collector_diameter_m){
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    yao_interception = (3/2)*Nr^2
    return(yao_interception)
}


# yao sedimentation

f_yao_sedimentation <- function(particle_density_kgm3, liq_density_kgm3, particle_diameter_m, visc_kgms, filtration_rate_ms){
    yao_sedimentation = f_Ng(particle_density_kgm3, liq_density_kgm3, particle_diameter_m, visc_kgms, filtration_rate_ms)
    return(yao_sedimentation)
}


# yao diffusion

f_yao_diffusion <- function(visc_kgms, particle_diameter_m, collector_diameter_m,filtration_rate_ms, temp_degc){
    temp_absolute = f_temp_abs(temp_degc = temp_degc)
    Pe = f_Pe(visc_kgms, particle_diameter_m, collector_diameter_m,filtration_rate_ms, temp_absolute)
    yao_diffusion = 4*Pe^-(2/3)
    return(yao_diffusion)
}

# RT interception

f_RT_interception<- function(porosity, visc_kgms, particle_diameter_m, filtration_rate_ms,collector_diameter_m){
    As = f_As(porosity = porosity)
    Na = f_Na(visc_kgms = visc_kgms, particle_diameter_m = particle_diameter_m, filtration_rate_ms = filtration_rate_ms)
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    RT_interception = As*((4/3)*Na)^(1/8)*Nr^(15/8)
    return(RT_interception)
}

# RT sedimentation

f_RT_sedimentation<- function(porosity, particle_diameter_m, collector_diameter_m, particle_density_kgm3, liq_density_kgm3, visc_kgms, filtration_rate_ms){
    As = f_As(porosity = porosity)
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    Ng = f_Ng(particle_density_kgm3 = particle_density_kgm3, liq_density_kgm3 = liq_density_kgm3, 
              particle_diameter_m = particle_diameter_m, visc_kgms = visc_kgms, filtration_rate_ms = filtration_rate_ms)
    
    RT_sedimentation = 0.00338*As*Nr^-0.4*Ng^1.2
    
    return(RT_sedimentation)
}

# RT diffusion

f_RT_diffusion<- function(porosity, visc_kgms, particle_diameter_m, collector_diameter_m, filtration_rate_ms, temp_degc){
    temp_absolute = f_temp_abs(temp_degc = temp_degc)
    As = f_As(porosity = porosity)
    Pe = f_Pe(visc_kgms, particle_diameter_m, collector_diameter_m,filtration_rate_ms, temp_absolute)
    RT_diffusion = 4*As^(1/3)*Pe^-(2/3)
    return(RT_diffusion)
}
    
# TE interception

f_TE_interception<- function(porosity, visc_kgms, particle_diameter_m, filtration_rate_ms,collector_diameter_m){
    As = f_As(porosity = porosity)
    Na = f_Na(visc_kgms = visc_kgms, particle_diameter_m = particle_diameter_m, filtration_rate_ms = filtration_rate_ms)
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    TE_interception = 0.55*As*Na^(1/8)*Nr^1.675
    return(TE_interception)
    
}
    
# Te sedimentation

f_TE_sedimentation<- function(particle_diameter_m, collector_diameter_m, temp_degc, particle_density_kgm3,
                              liq_density_kgm3,visc_kgms, filtration_rate_ms){
    temp_absolute = f_temp_abs(temp_degc = temp_degc)
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    Nvdw = f_Nvdw(temp_absolute)
    Ng = f_Ng(particle_density_kgm3 = particle_density_kgm3, liq_density_kgm3 = liq_density_kgm3, 
              particle_diameter_m = particle_diameter_m, visc_kgms = visc_kgms, filtration_rate_ms = filtration_rate_ms)
    
    TE_sedimentation = 0.22*Nr^-0.24*Nvdw^0.053*Ng^1.11
    return(TE_sedimentation)
}

# TE diffusion

f_TE_diffusion<- function(porosity, particle_diameter_m, collector_diameter_m,temp_degc,visc_kgms, filtration_rate_ms){
    temp_absolute = f_temp_abs(temp_degc)
    As = f_As(porosity = porosity)
    Nr = f_Nr(particle_diameter_m = particle_diameter_m, collector_diameter_m = collector_diameter_m)
    Nvdw = f_Nvdw(temp_absolute)
    Pe = f_Pe(visc_kgms, particle_diameter_m, collector_diameter_m,filtration_rate_ms, temp_absolute)
    TE_diffusion = 2.4*As^(1/3)*Nr^-0.081*Nvdw^0.052*Pe^-0.715
    return(TE_diffusion)
    
}

# yao_transport function

f_yao_transport_efficiency <- function(particle_diameter_m,collector_diameter_m,
                            visc_kgms, filtration_rate_ms, temp_degc,
                            particle_density_kgm3, liquid_density_kgm3){
    yao_interception = f_yao_interception(particle_diameter_m = particle_diameter_m,
                                          collector_diameter_m = collector_diameter_m)
    yao_sedimentation = f_yao_sedimentation(particle_density_kgm3 = particle_density_kgm3, 
                                            liq_density_kgm3 = liquid_density_kgm3,
                                            particle_diameter_m = particle_diameter_m, 
                                            visc_kgms = visc_kgms,
                                            filtration_rate_ms = filtration_rate_ms)
    yao_diffusion = f_yao_diffusion(visc_kgms = visc_kgms, 
                                    particle_diameter_m = particle_diameter_m, 
                                    collector_diameter_m = collector_diameter_m, 
                                    filtration_rate_ms = filtration_rate_ms, 
                                    temp_degc = temp_degc)
    yao_transport = yao_interception+yao_sedimentation+yao_diffusion 
    return(yao_transport)
}


# RT transport function

f_RT_transport_efficiency<- function(porosity, visc_kgms, particle_diameter_m, collector_diameter_m, filtration_rate_ms, temp_degc,
                          particle_density_kgm3, liq_density_kgm3){
    RT_interception =  f_RT_interception(porosity = porosity,visc_kgms = visc_kgms, 
                                         particle_diameter_m = particle_diameter_m, 
                                         filtration_rate_ms = filtration_rate_ms, 
                                         collector_diameter_m = collector_diameter_m)
    RT_sedimentation = f_RT_sedimentation(porosity = porosity, 
                                          particle_diameter_m = particle_diameter_m, 
                                          collector_diameter_m = collector_diameter_m, 
                                          particle_density_kgm3 = particle_density_kgm3, 
                                          liq_density_kgm3 = liq_density_kgm3, 
                                          visc_kgms = visc_kgms, 
                                          filtration_rate_ms = filtration_rate_ms)
    RT_diffusion = f_RT_diffusion(porosity = porosity, 
                                  visc_kgms = visc_kgms, 
                                  particle_diameter_m = particle_diameter_m, 
                                  collector_diameter_m = collector_diameter_m, 
                                  filtration_rate_ms = filtration_rate_ms, 
                                  temp_degc = temp_degc)
    RT_transport = RT_interception+RT_sedimentation+RT_diffusion
    return(RT_transport)
}


# TE transport function

f_TE_transport_efficiency<- function(porosity, particle_diameter_m, collector_diameter_m, temp_degc, visc_kgms, filtration_rate_ms,particle_density_kgm3, liq_density_kgm3 ){
    TE_interception = f_TE_interception(porosity = porosity, 
                                        visc_kgms = visc_kgms, 
                                        particle_diameter_m = particle_diameter_m, 
                                        filtration_rate_ms = filtration_rate_ms, 
                                        collector_diameter_m = collector_diameter_m)
    TE_sedimentation = f_TE_sedimentation(particle_diameter_m = particle_diameter_m, 
                                          collector_diameter_m = collector_diameter_m, 
                                          temp_degc = temp_degc, 
                                          particle_density_kgm3 = particle_density_kgm3, 
                                          liq_density_kgm3 = liq_density_kgm3, 
                                          visc_kgms = visc_kgms, 
                                          filtration_rate_ms = filtration_rate_ms)
    TE_diffusion = f_TE_diffusion(porosity = porosity, 
                                  particle_diameter_m = particle_diameter_m, 
                                  collector_diameter_m = collector_diameter_m, 
                                  temp_degc = temp_degc, 
                                  visc_kgms = visc_kgms, 
                                  filtration_rate_ms = filtration_rate_ms)
    TE_transport = TE_interception+TE_sedimentation+TE_diffusion
    return(TE_transport)
}



# removal rate calculation 11-36 

f_removal_rate <- function( porosity, transport_efficiency, attachment_efficiency, bed_depth_m, collector_diameter_m){
   C = exp((-3*(1-porosity)*transport_efficiency*attachment_efficiency*bed_depth_m)/(2*collector_diameter_m))
   return(1-C)
}

# test removal rate calculation 1-0.074

f_removal_rate(porosity = 0.5, 
    transport_efficiency = 1.39e-3, 
    attachment_efficiency = 1, 
    bed_depth_m = 1, 
    collector_diameter_m = 4e-4)
                          
                          
                          
                          
#test interception functions


f_yao_interception(particle_diameter_m = 1e-7,collector_diameter_m = 4e-4)
f_RT_interception(porosity = 0.5, visc_kgms = 1e-3, particle_diameter_m = 1e-7, filtration_rate_ms = 15/3600, collector_diameter_m = 4e-4)
f_TE_interception(porosity = 0.5, visc_kgms = 1e-3, particle_diameter_m = 1e-7, filtration_rate_ms = 15/3600, collector_diameter_m = 4e-4 )

f_yao_sedimentation(particle_density_kgm3 = 1050, liq_density_kgm3 = 998, particle_diameter_m = 1e-7, visc_kgms = 1e-3, filtration_rate_ms = 15/3600)
f_RT_sedimentation(porosity = 0.5, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, particle_density_kgm3 = 1050, liq_density_kgm3 = 998, visc_kgms = 1e-3, filtration_rate_ms = 15/3600)
f_TE_sedimentation(particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, temp_degc = 20, particle_density_kgm3 = 1050, liq_density_kgm3 = 998, visc_kgms = 1E-3, filtration_rate_ms = 15/3600)

f_yao_diffusion(visc_kgms = 1e-3, particle_diameter_m = 1e-7,collector_diameter_m = 4e-4, filtration_rate_ms = 15/3600, temp_degc = 20)
f_RT_diffusion(porosity = 0.5, visc_kgms = 1e-3, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, filtration_rate_ms = 15/3600, temp_degc = 20)
f_TE_diffusion(porosity = 0.5, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, temp_degc = 20, visc_kgms = 1e-3, filtration_rate_ms = 15/3600)


# test transport efficiency

te_yao<-f_yao_transport_efficiency(particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, visc_kgms = 1e-3, filtration_rate_ms = 15/3600, temp_degc = 20, particle_density_kgm3 = 1050, liquid_density_kgm3 = 998)
te_RT<-f_RT_transport_efficiency(porosity = 0.5, visc_kgms = 1e-3, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, filtration_rate_ms = 15/3600, temp_degc = 20, particle_density_kgm3 = 1050, liq_density_kgm3 = 998)
te_TE<-f_TE_transport_efficiency(porosity = 0.5, particle_diameter_m = 1e-7, collector_diameter_m = 4e-4, temp_degc = 20, visc_kgms = 1e-3, filtration_rate_ms = 15/3600, particle_density_kgm3 = 1050, liq_density_kgm3 = 998)


# test removal rates

f_removal_rate(porosity = 0.5, transport_efficiency = te_yao, attachment_efficiency = 1, bed_depth_m = 1, collector_diameter_m = 4e-4)
f_removal_rate(porosity = 0.5, transport_efficiency = te_RT, attachment_efficiency = 1, bed_depth_m = 1, collector_diameter_m = 4e-4)
f_removal_rate(porosity = 0.5, transport_efficiency = te_TE, attachment_efficiency = 1, bed_depth_m = 1, collector_diameter_m = 4e-4)

(1-0.07)

# set constants
sim_temp_degc<- 10
sim_collector_diameter<- 5e-4
sim_particle_density<- 1050
sim_liquid_density<- 998
sim_visc_kgms<- 1e-3
sim_filtration_rate<- 6/3600
sim_attachment<-1

rm(sim_bed_depth)



## Aggregate removal simulation ###


bed<-






## Mikol paper #####

roughing_collectors<-data.frame(layer = c("Lower layer (500mm)", "Upper layer (500mm)", "Lower layer (500mm)", "Lower layer (500mm)", "Upper layer (500mm)"), 
                                    media = c("Sand 0.6-1.2 mm", "Anthracite 1.7-2.5 mm","Sand 0.85-1.7 mm",
                                          "Filtralite HC 0.8-1.6 mm", "Filtralite NC 1.5-2.5 mm"),
                       sim_collector_diameter = c(0.5*(0.63+0.85),0.5*(1.75 + 1.85), 0.5*(0.85 + 0.95), 
                                                  0.98, 1.75)/1000,
                       sim_porosity = c(0.43, 0.5, 0.43, 0.55,0.60 ),
                       sim_bed_depth = c(0.5,0.5,0.5,0.5,0.5))

sim_matrix_rough<-crossing(media = roughing_collectors$media,
                           sim_particle_diameter = c(#1e-9,2e-9,3e-9,4e-9,5e-9,6e-9,7e-9,8e-9,9e-9,
                                                     #1e-8,2e-8,3e-8,4e-8,5e-8,6e-8,7e-8,8e-8,9e-8,
                                                     #1e-7,2e-7,3e-7,4e-7,5e-7,6e-7,7e-7,8e-7,9e-7#,
                                                     1e-6,2e-6,3e-6,4e-6,5e-6,6e-6,7e-6,8e-6,9e-6,
                                                     1e-5,2e-5,3e-5,4e-5,5e-5,6e-5,7e-5,8e-5,9e-5#,
                                                     #1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4
                                                     )
                           )%>%
                    left_join(., roughing_collectors)%>%
    mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter, 
                                                      visc_kgms = sim_visc_kgms, filtration_rate_ms = sim_filtration_rate, temp_degc = 10, 
                                                      particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
           RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                    collector_diameter_m = sim_collector_diameter, filtration_rate_ms = sim_filtration_rate, 
                                                    temp_degc = 10, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density),
           TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter,temp_degc = 10,
                                                    visc_kgms = sim_visc_kgms,
                                                    filtration_rate_ms = sim_filtration_rate, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density))%>%
    pivot_longer(cols = 7:9)%>%
    separate(col = name, into = c("model", "rate"), sep = "_")%>%
    pivot_wider(names_from = rate, values_from = value)%>%
    mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                          bed_depth_m = sim_bed_depth, collector_diameter_m = sim_collector_diameter),
            particle_micron = sim_particle_diameter*1000000)


ggplot(sim_matrix_rough%>%
           dplyr::filter(model == "RT")
       , aes(x = particle_micron, y = removal_rate, colour = media, group = interaction(model, media)))+
    geom_line()+
    scale_x_log10()+
    facet_grid(~layer)+
    theme_minimal()+
    geom_vline(xintercept = c(5,20))







collectors<- data.frame(media = c("Sand 0.5-1mm", "Anthracite 0.8-1.6mm", "Filtralite HC 0.5-1mm", "Filtralite NC 0.8-1.6mm", "Sand 0.85-1.7mm", "Filtralite NC (monomedia) 0.8-1.6mm"),
                        sim_collector_diameter = c(0.6, 0.98, 0.52, 0.98,1, 0.98)/1000,
                        sim_porosity = c(0.43, 0.5, 0.55,0.61, 0.43, 0.61),
                        sim_bed_depth = c(0.7, 0.3, 0.7, 0.3, 1, 1))

#### impact of temperature simulation

sim_matrix_ptcl_temp<- crossing(media = c("Sand 0.5-1mm", "Anthracite 0.8-1.6mm", "Filtralite HC 0.5-1mm", "Filtralite NC 0.8-1.6mm", "Sand 0.85-1.7mm", "Filtralite NC (monomedia) 0.8-1.6mm"),
                                sim_particle_diameter = c(1e-9,2e-9,3e-9,4e-9,5e-9,6e-9,7e-9,8e-9,9e-9,
                                                          1e-8,2e-8,3e-8,4e-8,5e-8,6e-8,7e-8,8e-8,9e-8,
                                                          1e-7,2e-7,3e-7,4e-7,5e-7,6e-7,7e-7,8e-7,9e-7,
                                                          1e-6,2e-6,3e-6,4e-6,5e-6,6e-6,7e-6,8e-6,9e-6,
                                                          1e-5,2e-5,3e-5,4e-5,5e-5,6e-5,7e-5,8e-5,9e-5,
                                                          1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4),
                                sim_temp = c(1,30))%>%
                        left_join(., collectors%>%dplyr::select(-sim_bed_depth))%>%
                        mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter, 
                                                                      visc_kgms = sim_visc_kgms, filtration_rate_ms = sim_filtration_rate, temp_degc = sim_temp, 
                                                                      particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
                                RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                                    collector_diameter_m = sim_collector_diameter, filtration_rate_ms = sim_filtration_rate, 
                                                                    temp_degc = sim_temp, particle_density_kgm3 = sim_particle_density, 
                                                                    liq_density_kgm3 = sim_liquid_density),
                                TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter,temp_degc = sim_temp,
                                                                    visc_kgms = sim_visc_kgms,
                                                                    filtration_rate_ms = sim_filtration_rate, particle_density_kgm3 = sim_particle_density, 
                                                                    liq_density_kgm3 = sim_liquid_density))%>%
                        pivot_longer(cols = 6:8)%>%
                        separate(col = name, into = c("model", "rate"), sep = "_")%>%
                        pivot_wider(names_from = rate, values_from = value)%>%
                        mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                                              bed_depth_m = 1, collector_diameter_m = sim_collector_diameter),
                                particle_micron = sim_particle_diameter*1000000)


ggplot(sim_matrix_ptcl_temp%>%
           dplyr::filter(model == "RT")
       , aes(x = particle_micron, y = removal_rate, colour = sim_temp, group = interaction(model, sim_temp)))+
    geom_line()+
    scale_x_log10()+
    facet_grid(~media)+
    theme_minimal()

                                    
#### impact of filtration rate simulation ####


sim_matrix_ptcl_rate<- crossing(media = c("Sand 0.5-1mm", "Anthracite 0.8-1.6mm", "Filtralite HC 0.5-1mm", "Filtralite NC 0.8-1.6mm", "Sand 0.85-1.7mm", "Filtralite NC (monomedia) 0.8-1.6mm"),
                                sim_particle_diameter = c(1e-9,2e-9,3e-9,4e-9,5e-9,6e-9,7e-9,8e-9,9e-9,
                                                          1e-8,2e-8,3e-8,4e-8,5e-8,6e-8,7e-8,8e-8,9e-8,
                                                          1e-7,2e-7,3e-7,4e-7,5e-7,6e-7,7e-7,8e-7,9e-7,
                                                          1e-6,2e-6,3e-6,4e-6,5e-6,6e-6,7e-6,8e-6,9e-6,
                                                          1e-5,2e-5,3e-5,4e-5,5e-5,6e-5,7e-5,8e-5,9e-5,
                                                          1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4),
                                sim_temp = 10,
                                filtration_rate = c(4,8)/3600)%>%
    left_join(., collectors%>%dplyr::select(-sim_bed_depth))%>%
    mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter, 
                                                      visc_kgms = sim_visc_kgms, filtration_rate_ms = filtration_rate, temp_degc = sim_temp, 
                                                      particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
           RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                    collector_diameter_m = sim_collector_diameter, filtration_rate_ms = filtration_rate, 
                                                    temp_degc = sim_temp, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density),
           TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter,temp_degc = sim_temp,
                                                    visc_kgms = sim_visc_kgms,
                                                    filtration_rate_ms = filtration_rate, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density))%>%
    pivot_longer(cols = 7:9)%>%
    separate(col = name, into = c("model", "rate"), sep = "_")%>%
    pivot_wider(names_from = rate, values_from = value)%>%
    mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                          bed_depth_m = 1, collector_diameter_m = sim_collector_diameter),
            particle_micron = sim_particle_diameter*1000000)


ggplot(sim_matrix_ptcl_rate, aes(x = particle_micron, y = removal_rate, colour = filtration_rate, 
                                 linetype = model, group = interaction(model,filtration_rate)))+
    geom_line()+
    scale_x_log10()+
    facet_wrap(~media)+
    theme_minimal()


## simulation of effective size

filter_sim_es<- crossing(media_size = c(0.6,0.52)/1000,
                         sim_porosity = c(0.43,0.55),
                         sim_particle_diameter = c(#1e-9,2e-9,3e-9,4e-9,5e-9,6e-9,7e-9,8e-9,9e-9,
                                                   1e-8,2e-8,3e-8,4e-8,5e-8,6e-8,7e-8,8e-8,9e-8,
                                                   1e-7,2e-7,3e-7,4e-7,5e-7,6e-7,7e-7,8e-7,9e-7,
                                                   1e-6,2e-6,3e-6,4e-6,5e-6,6e-6,7e-6,8e-6,9e-6,
                                                   1e-5,2e-5,3e-5,4e-5,5e-5,6e-5,7e-5,8e-5,9e-5#,
                                                   #1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4
                                                   ))%>%
    #left_join(.,collectors)%>%
    mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = media_size, 
                                                      visc_kgms = sim_visc_kgms, filtration_rate_ms = 6/3600, temp_degc = sim_temp_degc, 
                                                      particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
           RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                    collector_diameter_m = media_size, filtration_rate_ms = 6/3600, 
                                                    temp_degc = sim_temp_degc, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density),
           TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = media_size,temp_degc = sim_temp_degc,
                                                    visc_kgms = sim_visc_kgms,
                                                    filtration_rate_ms = 6/3600, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density))%>%
    pivot_longer(cols = 4:6)%>%
    separate(col = name, into = c("model", "rate"), sep = "_")%>%
    pivot_wider(names_from = rate, values_from = value)%>%
    mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                          bed_depth_m = 0.7, collector_diameter_m = media_size),
            particle_micron = sim_particle_diameter*1000000,
            effective_size = media_size*1000)



ggplot(filter_sim_es%>%
           dplyr::filter(model =="TE")
       , aes(x = particle_micron, y = removal_rate, colour = interaction(effective_size, sim_porosity) ))+
    geom_line()+
    scale_x_log10()+
    facet_wrap(~model)




filter_sim_matrix<- crossing( media = c("Sand 0.5-1mm", "Anthracite 0.8-1.6mm", "Filtralite HC 0.5-1mm", "Filtralite NC 0.8-1.6mm", "Sand 0.85-1.7mm", "Filtralite NC (monomedia) 0.8-1.6mm"),
                               sim_particle_diameter = c(1e-9,2e-9,3e-9,4e-9,5e-9,6e-9,7e-9,8e-9,9e-9,
                                                         1e-8,2e-8,3e-8,4e-8,5e-8,6e-8,7e-8,8e-8,9e-8,
                                                         1e-7,2e-7,3e-7,4e-7,5e-7,6e-7,7e-7,8e-7,9e-7,
                                                         1e-6,2e-6,3e-6,4e-6,5e-6,6e-6,7e-6,8e-6,9e-6,
                                                         1e-5,2e-5,3e-5,4e-5,5e-5,6e-5,7e-5,8e-5,9e-5,
                                                         1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4)
                              #sim_particle_diameter = c(2e-5, 1e-5, 1e-6, 1e-7)
                              )%>%
    left_join(.,collectors)%>%
    mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter, 
                                                                                  visc_kgms = sim_visc_kgms, filtration_rate_ms = sim_filtration_rate, temp_degc = sim_temp_degc, 
                                                                                  particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
                                       RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                                                collector_diameter_m = sim_collector_diameter, filtration_rate_ms = sim_filtration_rate, 
                                                                                temp_degc = sim_temp_degc, particle_density_kgm3 = sim_particle_density, 
                                                                                liq_density_kgm3 = sim_liquid_density),
                                       TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter,temp_degc = sim_temp_degc,
                                                                                visc_kgms = sim_visc_kgms,
                                                                                filtration_rate_ms = sim_filtration_rate, particle_density_kgm3 = sim_particle_density, 
                                                                                liq_density_kgm3 = sim_liquid_density))%>%
    pivot_longer(cols = 6:8)%>%
    separate(col = name, into = c("model", "rate"), sep = "_")%>%
    pivot_wider(names_from = rate, values_from = value)%>%
    mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                          bed_depth_m = sim_bed_depth, collector_diameter_m = sim_collector_diameter),
            particle_micron = sim_particle_diameter*1000000)



ggplot(filter_sim_matrix%>%dplyr::filter(model == "TE"), aes(x = sim_particle_diameter, y = removal_rate, colour = media))+
    geom_line()+
    scale_x_log10()+
    facet_grid(~sim_bed_depth)


ggplot(filter_sim_matrix%>%dplyr::filter(model == "TE"), aes(x = sim_collector_diameter*1000, y = removal_rate, colour = media))+
    geom_line()+
    facet_grid(particle_micron~sim_bed_depth)




backwash_sim_matrix<-crossing(sim_collector_diameter = 0.55,
                              sim_bed_depth = c(1,1.1,1.2),
                              sim_particle_diameter = c(1e-4,2e-4,3e-4,4e-4,5e-4,6e-4,7e-4,8e-4,9e-4,
                                                        1e-3,2e-3,3e-3,4e-3,5e-3,6e-3,7e-3,8e-3,9e-3),
                              sim_backwash_rate = c(23))%>%
    mutate(sim_porosity = (sim_bed_depth-(1-0.43)/1) )%>%
    mutate(yao_transport = f_yao_transport_efficiency(particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter, 
                                                      visc_kgms = sim_visc_kgms, filtration_rate_ms = sim_backwash_rate, temp_degc = sim_temp_degc, 
                                                      particle_density_kgm3 = sim_particle_density, liquid_density_kgm3 = sim_liquid_density),
           RT_transport = f_RT_transport_efficiency(porosity = sim_porosity, visc_kgms = sim_visc_kgms, particle_diameter_m = sim_particle_diameter, 
                                                    collector_diameter_m = sim_collector_diameter, filtration_rate_ms = sim_backwash_rate, 
                                                    temp_degc = sim_temp_degc, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density),
           TE_transport = f_TE_transport_efficiency(porosity = sim_porosity,particle_diameter_m = sim_particle_diameter, collector_diameter_m = sim_collector_diameter,temp_degc = sim_temp_degc,
                                                    visc_kgms = sim_visc_kgms,
                                                    filtration_rate_ms = sim_backwash_rate, particle_density_kgm3 = sim_particle_density, 
                                                    liq_density_kgm3 = sim_liquid_density))%>%
    pivot_longer(cols = 6:8)%>%
    separate(col = name, into = c("model", "rate"), sep = "_")%>%
    pivot_wider(names_from = rate, values_from = value)%>%
    mutate( removal_rate = f_removal_rate(porosity = sim_porosity, transport_efficiency = transport,attachment_efficiency = sim_attachment, 
                                          bed_depth_m = sim_bed_depth, collector_diameter_m = sim_collector_diameter),
            particle_micron = sim_particle_diameter*1000000)               


ggplot(backwash_sim_matrix, aes(x = sim_particle_diameter*1000, y = removal_rate, colour = model))+
    geom_point()+
    facet_wrap(~sim_bed_depth)+
    scale_x_log10()





