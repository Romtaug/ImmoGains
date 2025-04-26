options(shinyapps.locale.cache = FALSE)

# Fonction de calcul des entrées
calculEntree <- function(loyer_potentiel_hors_charge, charges_demandees_au_locataire) {
  # Calculer le total des entrées
  total_entrees <- loyer_potentiel_hors_charge + charges_demandees_au_locataire
  return(total_entrees)
}

################################################################################


# Fonction de calcul des sorties non déductibles
calculSortieNonDeductible <- function(remboursement_capital, frais_de_notaire, frais_et_hypotheque) {
  # Calculer le total des sorties non déductibles
  total_sortie_nondeductible <- remboursement_capital + frais_de_notaire + frais_et_hypotheque
  return(total_sortie_nondeductible)
}

################################################################################


# Fonction de calcul des sorties déductibles
calculSortieDeductible <- function(interets_emprunt, assurance_credit, taxe_fonciere,
                                   abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                   abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                   autres_frais, travaux, amortissement) {
  # Calculer le total des sorties déductibles
  total_sortie_deductible <- interets_emprunt + assurance_credit + taxe_fonciere + abonnement_electricite_gaz +
    charges_copropriete + assurance_PNO + abonnement_internet_TV + cotisation_fonciere_entreprise +
    frais_comptable + autres_frais + travaux + amortissement
  return(total_sortie_deductible)
}

################################################################################

# Fonction de calcul des sorties
calculSortie <- function(remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                         interets_emprunt, assurance_credit, taxe_fonciere,
                         abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                         abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                         autres_frais, travaux, amortissement) {
  # Calculer le total des sorties non déductibles
  total_sortie_nondeductible <- calculSortieNonDeductible(remboursement_capital, frais_de_notaire, frais_et_hypotheque)
  
  # Calculer le total des sorties déductibles
  total_sortie_deductible <- calculSortieDeductible(interets_emprunt, assurance_credit, taxe_fonciere,
                                                    abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                                    abonnement_internet_TV, cotisation_fonciere_entreprise,
                                                    frais_comptable, autres_frais, travaux, amortissement)
  
  # Calculer le total des sorties
  total_sorties <- total_sortie_nondeductible + total_sortie_deductible
  return(total_sorties)
}

# Fonction de calcul du bénéfice
calculBenefice <- function(loyer_potentiel_hors_charge, charges_demandees_au_locataire,
                           remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                           interets_emprunt, assurance_credit, taxe_fonciere,
                           abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                           abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                           autres_frais, travaux, amortissement) {
  
  # Calculer le total des entrées
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)
  
  # Calculer le total des sorties
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                interets_emprunt, assurance_credit, taxe_fonciere,
                                abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                autres_frais, travaux, amortissement)
  
  # Calculer le bénéfice annuel
  benefice_annuel <- total_entrees - total_sorties
  
  return(benefice_annuel)
}


########
#A corriger
# Fonction de calcul des impôts
calculImpots <- function(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, 
                         remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                         interets_emprunt, assurance_credit, taxe_fonciere, 
                         abonnement_electricite_gaz, charges_copropriete, assurance_PNO, 
                         abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,  
                         autres_frais, travaux, amortissement) {
  
  # Calculer le total des entrées
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)*12
  
  # Calculer le total des sorties
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque, 
                                interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz, 
                                charges_copropriete, assurance_PNO, abonnement_internet_TV, 
                                cotisation_fonciere_entreprise, frais_comptable, autres_frais, travaux, amortissement)*12
  
  sortie_deductible <- calculSortieDeductible(interets_emprunt, assurance_credit, taxe_fonciere,
                                              abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                              abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                              autres_frais, travaux, amortissement)*12
  
  # Définir le bénéfice comme la différence entre les entrées et les sorties
  benefice_annuel <- total_entrees - total_sorties
  
  
  # Calculer l'impôt en fonction du régime d'imposition choisi
  if (choix == "1") {
    # Dans le régime réel, l'impôt est calculé sur le revenu net après déduction des charges et des dépenses déductibles
    revenus_imposables <- benefice_annuel - sortie_deductible
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
    #cat("Revenu imposable :", revenus_imposables)
    # Trouver le taux d'imposition correspondant à la tranche de revenu
    if (revenus_imposables <= 10084) {
      taux_imposition <- 0
    } else if (revenus_imposables <= 25710) {
      taux_imposition <- 0.11
    } else if (revenus_imposables <= 73516) {
      taux_imposition <- 0.30
    } else if (revenus_imposables <= 158122) {
      taux_imposition <- 0.41
    } else {
      taux_imposition <- 0.45
    }
    
    #impot <- revenus_imposables * taux_imposition
    impot <- revenus_imposables * 0.3
  } 
  else if (choix == "2") {
    # Dans le régime micro-BIC, l'impôt est calculé sur 50% du revenu brut
    revenus_imposables <- total_entrees * 0.7
    #cat("Revenu imposable :", revenus_imposables)
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
    if (revenus_imposables <= 10084) {
      taux_imposition <- 0
    } else if (revenus_imposables <= 25710) {
      taux_imposition <- 0.11
    } else if (revenus_imposables <= 73516) {
      taux_imposition <- 0.30
    } else if (revenus_imposables <= 158122) {
      taux_imposition <- 0.41
    } else {
      taux_imposition <- 0.45
    }
    
    #impot <- revenus_imposables * taux_imposition
    impot <- revenus_imposables * 0.3
    
  } 
  else {
    print("Choix invalide. Veuillez entrer 1 pour LMNP réel ou 2 pour Micro-foncier.")
    impot <- 0
  }
  #cat("Impôt :", impot, "\n\n")
  return(impot)
}
calculTauxImpots <- function(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, 
                             remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                             interets_emprunt, assurance_credit, taxe_fonciere, 
                             abonnement_electricite_gaz, charges_copropriete, assurance_PNO, 
                             abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,  
                             autres_frais, travaux, amortissement) {
  
  # Calculer le total des entrées
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)*12
  
  # Calculer le total des sorties
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque, 
                                interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz, 
                                charges_copropriete, assurance_PNO, abonnement_internet_TV, 
                                cotisation_fonciere_entreprise, frais_comptable, autres_frais, travaux, amortissement)*12
  
  sortie_deductible <- calculSortieDeductible(interets_emprunt, assurance_credit, taxe_fonciere,
                                              abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                              abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                              autres_frais, travaux, amortissement)*12
  
  # Définir le bénéfice comme la différence entre les entrées et les sorties
  benefice_annuel <- total_entrees - total_sorties
  
  # Calculer le revenu imposable et le taux d'imposition en fonction du régime d'imposition choisi
  if (choix == "1") {
    # Dans le régime réel
    revenus_imposables <- benefice_annuel - sortie_deductible
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
    #cat("Revenu imposable :", revenus_imposables)
    # Trouver le taux d'imposition correspondant à la tranche de revenu
    if (revenus_imposables <= 10084) {
      taux_imposition <- 0
    } else if (revenus_imposables <= 25710) {
      taux_imposition <- 0.11
    } else if (revenus_imposables <= 73516) {
      taux_imposition <- 0.30
    } else if (revenus_imposables <= 158122) {
      taux_imposition <- 0.41
    } else {
      taux_imposition <- 0.45
    }
  } 
  else if (choix == "2") {
    # Dans le régime micro-BIC
    revenus_imposables <- total_entrees * 0.5
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
    #cat("Revenu imposable :", revenus_imposables)
    if (revenus_imposables <= 10084) {
      taux_imposition <- 0
    } else if (revenus_imposables <= 25710) {
      taux_imposition <- 0.11
    } else if (revenus_imposables <= 73516) {
      taux_imposition <- 0.30
    } else if (revenus_imposables <= 158122) {
      taux_imposition <- 0.41
    } else {
      taux_imposition <- 0.45
    }
  } 
  else {
    print("Choix invalide. Veuillez entrer 1 pour le régime réel ou 2 pour le régime micro-BIC.")
    taux_imposition <- NA
  }
  #cat("Taux d'imposition :", taux_imposition, "\n\n")
  return(taux_imposition)
}



calculRevenuImposable <- function(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, 
                                  remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                  interets_emprunt, assurance_credit, taxe_fonciere, 
                                  abonnement_electricite_gaz, charges_copropriete, assurance_PNO, 
                                  abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,  
                                  autres_frais, travaux, amortissement) {
  
  # Calculer le total des entrées
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)*12
  
  # Calculer le total des sorties
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque, 
                                interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz, 
                                charges_copropriete, assurance_PNO, abonnement_internet_TV, 
                                cotisation_fonciere_entreprise, frais_comptable, autres_frais, travaux, amortissement)*12
  
  sortie_deductible <- calculSortieDeductible(interets_emprunt, assurance_credit, taxe_fonciere,
                                              abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                              abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                              autres_frais, travaux, amortissement)*12
  
  # Définir le bénéfice comme la différence entre les entrées et les sorties
  benefice_annuel <- total_entrees - total_sorties
  
  # Calculer le revenu imposable en fonction du régime d'imposition choisi
  if (choix == "1") {
    # Dans le régime réel, le revenu imposable est calculé comme le bénéfice annuel moins les sorties déductibles
    revenus_imposables <- benefice_annuel - sortie_deductible
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
  } 
  else if (choix == "2") {
    # Dans le régime micro-BIC, le revenu imposable est calculé sur 50% du revenu brut
    revenus_imposables <- total_entrees * 0.5
    if (revenus_imposables < 0) {
      revenus_imposables <- 0
    }
  } else {
    print("Choix invalide. Veuillez entrer 1 pour le régime réel ou 2 pour le régime micro-BIC.")
    revenus_imposables <- NA
  }
  
  return(revenus_imposables)
}

# Fonction de calcul du cashflow
calculCashflow <- function(loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                           frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                           charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                           autres_frais, travaux, amortissement, choix) {
  
  # Calculer le total des entrées
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)
  
  # Calculer le total des sorties
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                                charges_copropriete, assurance_PNO, abonnement_internet_TV,
                                cotisation_fonciere_entreprise, frais_comptable, autres_frais, travaux, amortissement)
  
  # Calculer les impôts
  impot <- calculImpots(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                        frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                        charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise,
                        frais_comptable, autres_frais, travaux, amortissement)
  
  # Calculer le cashflow
  cashflow <- total_entrees - total_sorties - impot/12
  
  return(cashflow)
}



# Fonction de calcul de la rentabilité
calculRentabilite <- function(loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                              frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                              charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                              autres_frais, travaux, amortissement, choix, cout_total_investissement) {
  
  # Calculer le cashflow
  cashflow <- calculCashflow(loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                             frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                             charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                             autres_frais, travaux, amortissement, choix)
  
  # Calculer la rentabilité nette
  rentabilite_nette <- (cashflow * 12 / cout_total_investissement)
  
  return(rentabilite_nette)
}

################################################################################

creerDatasetCashflow <- function(loyer_potentiel_hors_charge, charges_demandees_au_locataire,
                                 remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                 interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                                 charges_copropriete, assurance_PNO, abonnement_internet_TV,
                                 cotisation_fonciere_entreprise, frais_comptable, autres_frais,
                                 travaux, amortissement, choix, cout_total_investissement) {
  
  total_entrees <- calculEntree(loyer_potentiel_hors_charge, charges_demandees_au_locataire)
  
  total_sortie_nondeductible <- calculSortieNonDeductible(remboursement_capital, frais_de_notaire, frais_et_hypotheque)
  
  total_sortie_deductible <- calculSortieDeductible(interets_emprunt, assurance_credit, taxe_fonciere,
                                                    abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                                    abonnement_internet_TV, cotisation_fonciere_entreprise,
                                                    frais_comptable, autres_frais, travaux, amortissement)
  
  total_sorties <- calculSortie(remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                                charges_copropriete, assurance_PNO, abonnement_internet_TV,
                                cotisation_fonciere_entreprise, frais_comptable, autres_frais, travaux, amortissement)
  
  benefice_annuel <- calculBenefice(loyer_potentiel_hors_charge, charges_demandees_au_locataire,
                                    remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                    interets_emprunt, assurance_credit, taxe_fonciere,
                                    abonnement_electricite_gaz, charges_copropriete, assurance_PNO,
                                    abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                    autres_frais, travaux, amortissement)
  
  revenu_imposable <- calculRevenuImposable(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, 
                                            remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                                            interets_emprunt, assurance_credit, taxe_fonciere, 
                                            abonnement_electricite_gaz, charges_copropriete, assurance_PNO, 
                                            abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,  
                                            autres_frais, travaux, amortissement)
  
  impot <- calculImpots(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                        frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                        charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise,
                        frais_comptable, autres_frais, travaux, amortissement)
  
  cashflow <- calculCashflow(loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                             frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                             charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                             autres_frais, travaux, amortissement, choix)
  
  rentabilite_nette <- calculRentabilite(loyer_potentiel_hors_charge, charges_demandees_au_locataire, remboursement_capital, frais_de_notaire,
                                         frais_et_hypotheque, interets_emprunt, assurance_credit, taxe_fonciere, abonnement_electricite_gaz,
                                         charges_copropriete, assurance_PNO, abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,
                                         autres_frais, travaux, amortissement, choix, cout_total_investissement)
  taux <- calculTauxImpots(choix, loyer_potentiel_hors_charge, charges_demandees_au_locataire, 
                           remboursement_capital, frais_de_notaire, frais_et_hypotheque,
                           interets_emprunt, assurance_credit, taxe_fonciere, 
                           abonnement_electricite_gaz, charges_copropriete, assurance_PNO, 
                           abonnement_internet_TV, cotisation_fonciere_entreprise, frais_comptable,  
                           autres_frais, travaux, amortissement)
  
  
  
  data <- data.frame(loyer_potentiel_hors_charge = loyer_potentiel_hors_charge,
                     charges_demandees_au_locataire = charges_demandees_au_locataire,
                     TOTAL_ENTREES_MENSUELLES = total_entrees,
                     
                     remboursement_capital = remboursement_capital,
                     frais_de_notaire = frais_de_notaire,
                     frais_et_hypotheque = frais_et_hypotheque,
                     TOTAL_SORTIE_NON_DEDUCTIBLE_MENSUELLES = total_sortie_nondeductible,
                     
                     interets_emprunt = interets_emprunt,
                     assurance_credit = assurance_credit,
                     taxe_fonciere = taxe_fonciere,
                     abonnement_electricite_gaz = abonnement_electricite_gaz,
                     charges_copropriete = charges_copropriete,
                     assurance_PNO = assurance_PNO,
                     abonnement_internet_TV = abonnement_internet_TV,
                     cotisation_fonciere_entreprise = cotisation_fonciere_entreprise,
                     frais_comptable = frais_comptable,
                     autres_frais = autres_frais,
                     travaux = travaux,
                     amortissement = amortissement,
                     TOTAL_SORTIE_DEDUCTIBLE_MENSUELLES = total_sortie_deductible,
                     
                     TOTAL_SORTIES_MENSUELLES = total_sorties,
                     BENEFICE_MENSUEL = benefice_annuel,
                     REVENU_IMPOSABLE_ANNUEL = revenu_imposable,
                     #TAUX_IMPOSITION = taux,
                     IMPOT_ANNUEL = impot,
                     CASHFLOW_MENSUEL = cashflow,
                     cout_total_investissement = cout_total_investissement,
                     RENTABILITE_NETTE_ANNUELLE = paste0(round(rentabilite_nette * 100, 2), " %")
  )
  
  
  # Transposition du dataframe
  resultat_dataset_transpose <- as.data.frame(t(data))
  
  # Définition des index des lignes à copier
  indices <- c("TOTAL_ENTREES_MENSUELLES", "TOTAL_SORTIE_NON_DEDUCTIBLE_MENSUELLES", "TOTAL_SORTIE_DEDUCTIBLE_MENSUELLES", 
               "TOTAL_SORTIES_MENSUELLES", "BENEFICE_MENSUEL", "REVENU_IMPOSABLE_ANNUEL", "IMPOT_ANNUEL", "CASHFLOW_MENSUEL", 
               "RENTABILITE_NETTE_ANNUELLE")
  
  # Copie des valeurs de V1 à Calculés pour les lignes spécifiées
  resultat_dataset_transpose$Calculés <- lapply(rownames(resultat_dataset_transpose), function(x){
    if(x %in% indices){
      return(resultat_dataset_transpose[x, "V1"])
    } else {
      return(NA)
    }
  })
  
  # Remplacement des valeurs en V1 par NA si elles sont déjà dans Calculés
  resultat_dataset_transpose$V1 <- ifelse(!is.na(resultat_dataset_transpose$Calculés), NA, resultat_dataset_transpose$V1)
  
  # Renommage des colonnes
  colnames(resultat_dataset_transpose) <- c("Valeurs entrées", "Valeurs calculées")
  
  return(resultat_dataset_transpose)
}


################################################################################
regimeFiscal <- function(choix) {
  
  # Vérifier la valeur du choix
  if (choix == 1) {
    return("régime réel")
  } 
  else if (choix == 2) {
    return("régime micro-BIC")
  }
}

ouvrirContract <- function(){
  url <- "https://www.bailfacile.fr/contrats/bail-meuble#7-quelles-sont-les-regles-de-fonctionnemen"
  actionLink("ouvrirContrat", "Modèle de contrat", href = "https://www.bailfacile.fr/contrats/bail-meuble#7-quelles-sont-les-regles-de-fonctionnemen", target = "_blank", class = "btn-primary btn-lg")
  browseURL(url)
}

################################################################################

library(shiny)

ui <- fluidPage(
  titlePanel("ImmoGains"),
  
  tags$head(tags$meta(charset = "UTF-8")),
  
  
  tags$head(
    tags$link(rel = "shortcut icon", href = "logo_immo.png", type = "image/png")
  ),
  div(
    tags$img(src = "logo_immo.png", height = 45, style = "vertical-align:middle; margin-right: 10px;"),
    style = "position: absolute; top: 15px; right: 15px;"
  ),
  
  tags$p(style = "font-weight:bold;", 
         "Découvrez la maîtrise ultime de vos investissements immobiliers grâce à notre outil dédié aux Loueurs en Meublé Non Professionnel (LMNP). 
         Notre calculateur vous guide à travers toutes les étapes clés d'un investissement LMNP, en vous fournissant des conseils et des ressources utiles. 
         Optimisez les bénéfices fiscaux du régime LMNP, estimez avec précision votre cashflow et la rentabilité de vos investissements, et faites des choix d'investissement éclairés. 
         De plus, nous vous conseillons des sites web fiables pour faciliter chaque étape de votre investissement. 
         Avec notre outil, transformez des données complexes en décisions d'investissement éclairées et faites prospérer votre portefeuille immobilier."),
  
  tags$div(style = "font-family: Arial, sans-serif; margin: 20px;",
           
           tags$h3(style = "color: #2a9df4;", "Dans quelle ville investir ?"),
           
           tags$p(tags$strong("Objectif du bien :")),
           
           tags$ul(
             tags$li(tags$strong("Vérifiez la démographie locale :"), 
                     " Choisissez une ville de plus de 20 000 habitants avec une croissance démographique positive."),
             tags$li(tags$strong("Localisation stratégique :"), 
                     " Sélectionnez des villes avec une forte demande locative, à moins d’une heure de chez vous, dans des quartiers en développement.️"),
             tags$li(tags$strong("Analyse économique :"), 
                     " Recherchez des villes avec un faible taux de chômage et un fort taux d'activité."),
             tags$li(tags$strong("Tension locative :"), 
                     " Assurez-vous que ", tags$strong("60 % ou plus"), " des habitants sont locataires pour garantir une demande stable."),
             tags$li(tags$strong("Ciblez les biens en demande :"), 
                     " Studios et T2, généralement plus demandés dans les centres-villes."),
             tags$li(tags$strong("Calcul du rendement locatif :"), 
                     " Utilisez la formule : (Loyer mensuel x 12) / Prix d'achat x 100 et visez une rentabilité supérieure à 8 %."),
             tags$li(tags$strong("Rentabilité et potentiel de croissance :"), 
                     " Une rentabilité élevée attire davantage d’investisseurs, augmentant ainsi le potentiel des prix à si la ville se développe."),
             tags$li(tags$strong("Test d'intérêt locatif :"), 
                     " Publiez une annonce fictive pour tester la demande sur le type de bien que vous envisagez d’acheter.")
           ),
           
           tags$p("Vous ne savez pas où investir ?", 
                  tags$strong("GPT peut vous orienter avec précision. 🤖"), 
                  " Utilisez ce prompt pour découvrir les meilleures villes selon vos critères.", 
                  tags$br(),
                  "De plus, vous pouvez recevoir directement à l'adresse ", 
                  tags$a(href="mailto:taugourdea@cy-tech.fr", "taugourdea@cy-tech.fr"), 
                  " une liste des annonces Leboncoin répondant à vos critères avec les meilleures opportunités et rentabilités. 📩"),
           
           
           # Input field for city
           textInput("user_city", "Indiquez votre lieu de travail/domicile :", placeholder = "Votre lieu"),
           
           tags$p(
             tags$em("Prompt généré :"),
             tags$strong(textOutput("generated_prompt")),
             style = "display: inline-block; margin-right: 10px;"
           ),
           
           # Button to copy prompt and open GPT + Meilleurs Agents link on the same line
           tags$div(
             style = "display: flex; align-items: center;",  # Flexbox for better alignment
             tags$a(id = "gptButton", href = "https://chat.openai.com", target = "_blank", class = "btn btn-primary btn-lg", "Ouvrir GPT et Copier le Prompt 💾"),
             tags$p(style = "margin-left: 15px;",  # Adds space between button and the link
                    "Vérifiez toutes ces informations sur", 
                    tags$a(href = "https://www.meilleursagents.com/prix-immobilier/?gad_source=1&gclid=Cj0KCQjw6oi4BhD1ARIsAL6pox3kCKxuo9veX2qqbntkIuJg2pnY_8b88-it5qq3cz5t3lDsqSsD2pgaAviEEALw_wcB&gclsrc=aw.ds", 
                           target = "_blank", 
                           style = "color: #2a9df4; text-decoration: none; font-weight: bold;",
                           "Meilleurs Agents 🌐"))
           ),
           br(),
           tags$li("📊 ", HTML("<strong>BusinessTracker</strong>"), " pour apprendre à gérer vos finances personnelles. Vous pouvez retourner sur le simulateur d'intérêts composés : ", a("Simulateur d'intérêts composés", href="https://businesstracker.netlify.app/simulateurinteretscomposes")),
           tags$li("📈 ", HTML("<strong>FinanceFinder</strong>"), " pour investir sur différentes classes d'actifs et prendre des décisions éclairées en fonction de vos objectifs financiers. Explorez vos options dès maintenant ici : ", a("Outil d'aide à la décision", href="https://romtaugs.shinyapps.io/FinanceFinder/")),
           tags$li("🏡 ", HTML("<strong>ÉcoReno-GRDF</strong>"), " pour simuler la rénovation énergétique de votre logement et découvrir les aides disponibles afin de garantir et améliorer votre DPE : ", a("Simulateur de rénovation", href="https://ecoreno-grdf.netlify.app/"))
  ),
  
  # JavaScript code for copying text to clipboard
  tags$script(HTML("
    document.getElementById('gptButton').onclick = function() {
      // Get the prompt from the R Shiny server
      var promptText = document.getElementById('generated_prompt').innerText;
      
      // Create a temporary textarea element
      var tempInput = document.createElement('textarea');
      tempInput.style.position = 'absolute';
      tempInput.style.left = '-9999px';
      tempInput.value = promptText;
      document.body.appendChild(tempInput);
      
      // Copy the text
      tempInput.select();
      document.execCommand('copy');
      document.body.removeChild(tempInput);
      
      // Alert the user
      alert('Le prompt a été copié dans le presse-papiers. ✂️📋');
    };
  ")),
  
  ##############################################################################
  sidebarLayout(
    sidebarPanel(
      
      
      # Onglets pour les différentes sections
      tabsetPanel(
        
        tabPanel(
          "Étapes pour investir",
          h3("Suivez ces étapes pour investir"),
          
          h4("Étape 1 : Établir votre budget et votre stratégie d'investissement"),
          p("Il s'agit de l'étape préliminaire permettant de déterminer quelle est la meilleure option pour vous, en prenant en compte ", strong("votre budget"), ", ", strong("votre potentiel crédit"), ", ", strong("vos objectifs d'investissement"), " et ", strong("votre tolérance au risque"), ". Vous pouvez utiliser ", tags$a(href = "https://www.meilleurtaux.com/", target = "_blank", "MeilleurTaux"), " pour déterminer le meilleur taux d'emprunt."),
          
          h4("Étape 2 : Rechercher le bon bien immobilier"),
          p("Pour que l'investissement ", strong("LMNP"), " soit rentable, il est crucial de choisir le bon bien immobilier, dans l'idéal proche de chez vous pour simplifier la gestion. Cela peut être un ", strong("appartement"), ", une ", strong("maison"), ", une ", strong("résidence étudiante"), ", une ", strong("résidence de tourisme"), ", une ", strong("résidence pour personnes âgées"), ", etc. Le choix du bien dépend de vos objectifs et de votre budget. Utilisez des plateformes comme ", tags$a(href = "https://www.meilleursagents.com/", target = "_blank", "MeilleursAgents"), " ou ", tags$a(href = "https://www.seloger.com/", target = "_blank", "SeLoger"), " pour faciliter votre recherche."),
          
          h4("Étape 3 : Évaluer la rentabilité"),
          p("Une fois le bien identifié, il est crucial d'évaluer ", strong("sa rentabilité"), ". Pour cela, vous pouvez utiliser un simulateur comme celui de ", tags$a(href = "https://www.meilleurtaux.com/", target = "_blank", "MeilleurTaux"), ""),
          
          h4("Étape 4 : Acheter le bien immobilier"),
          p("Si le bien identifié correspond à ", strong("vos critères"), " et présente ", strong("une bonne rentabilité"), ", vous pouvez procéder à son achat. Il est recommandé de consulter un notaire pour vous accompagner dans cette démarche."),
          
          h4("Étape 5 : Meubler le bien"),
          p("Pour bénéficier du régime ", strong("LMNP"), ", il faut que le bien soit ", strong("loué meublé"), ". Assurez-vous de fournir tous les meubles nécessaires pour permettre au locataire de vivre convenablement dans le logement. Pour l'ameublement, vous pouvez utiliser des sites comme ", tags$a(href = "https://www.geev.com/", target = "_blank", "GEEV"), ", ", tags$a(href = "https://www.leboncoin.fr/", target = "_blank", "LebonCoin"), " ou ", tags$a(href = "https://www.ikea.com/fr/fr/", target = "_blank", "Ikea"), ""),
          p("Voici la liste minimale des équipements obligatoires selon le décret n°2015-981 :"),
          tags$ul(
            tags$li("Literie comprenant couette ou couverture"),
            tags$li("Dispositif d'occultation des fenêtres dans les chambres (rideaux, volets)"),
            tags$li("Plaques de cuisson"),
            tags$li("Four ou four à micro-ondes"),
            tags$li("Réfrigérateur avec compartiment congélateur ou congélateur séparé"),
            tags$li("Vaisselle nécessaire pour la prise des repas (assiettes, verres, couverts, etc.)"),
            tags$li("Ustensiles de cuisine (poêles, casseroles, spatules, etc.)"),
            tags$li("Table et sièges"),
            tags$li("Étagères de rangement"),
            tags$li("Luminaires"),
            tags$li("Matériel d'entretien ménager adapté au logement (aspirateur, balai, serpillère si nécessaire)")
          ),
          h4("Étape 6 : Inscription au régime LMNP"),
          p("Une fois le bien meublé, il faut vous ", strong("déclarer comme loueur en meublé non professionnel"), " auprès du greffe du tribunal de commerce de votre ville pour obtenir un ", strong("numéro SIRET"), ""),
          
          h4("Étape 7 : Trouver un locataire et gérer la location"),
          p("Cela consiste à ", strong("trouver un locataire"), ", ", strong("gérer le bail"), ", ", strong("percevoir les loyers"), ", résoudre les éventuels problèmes, etc. Vous pouvez faire cela vous-même ou recourir aux services d'une société de gestion immobilière. Pour la location, vous pouvez utiliser des sites comme ", tags$a(href = "https://www.leboncoin.fr/", target = "_blank", "LebonCoin"), " ou ", tags$a(href = "https://www.seloger.com/", target = "_blank", "SeLoger"), ""),
          
          h4("Étape 8 : Déclaration fiscale"),
          p("Chaque année, il vous faudra déclarer vos ", strong("revenus locatifs"), " dans votre déclaration d'impôt sur le revenu. En fonction du régime choisi, ", strong("micro-BIC"), " ou ", strong("régime réel"), ", vous bénéficierez d'un ", strong("abattement forfaitaire de 50%"), " pour frais et charges, ou vous pourrez déduire vos charges réelles et ", strong("amortir votre bien et vos meubles"), ". Une fiche d'impôt vous sera envoyée une fois par an."),
          
          h4("Étape 9 : Gestion des charges et assurances obligatoires"),
          p("Cette étape implique la gestion de différentes charges et assurances :"),
          p("1. ", strong("Charges"), " : Taxe foncière, charges de copropriété, frais d'entretien, charges non récupérables sur le locataire."),
          p("2. ", strong("Assurances"), " : Assurance Propriétaire Non Occupant (PNO), vérification de l'assurance multirisque habitation du locataire, éventuellement une Garantie des Loyers Impayés (GLI)."),
          p("Il est conseillé de comparer les offres pour minimiser ces coûts. Utilisez ", tags$a(href = "https://www.selectra.info/", target = "_blank", "Selectra"), " pour les charges et ", tags$a(href = "https://www.lesfurets.com/", target = "_blank", "LesFurets"), " pour l'assurance."),
          
          h4("Étape 10 : Suivi de l'investissement"),
          p("Une fois que tout est en place, il est important de ", strong("surveiller votre investissement"), " afin de garantir ", strong("sa rentabilité"), " et d'ajuster votre stratégie si nécessaire via notre site."),
          
          h4("Étape Bonus : Consultation avec un conseiller"),
          p("Il est recommandé de consulter un ", strong("conseiller en gestion de patrimoine"), " ou un ", strong("expert-comptable spécialisé en LMNP"), " pour optimiser votre investissement. L'objectif principal est d'obtenir un ", strong("cashflow positif"), " qui permettra de répéter ce genre d'opération, simuler le sur notre site."),
          br()
        ),
        ########################################################################
        ########################################################################
        tabPanel(
          'Étude de marché',
          br(),
          textInput("nom_annonce", "Nom de l'annonce", value = "Longwy"),
          
          h3("Étape 1 : Choisissez le bien !"),
          p("Trouvez un bien à ", strong("potentiel locatif"), ", idéalement nécessitant des ", strong("travaux énergétiques (entre D et G)"), ", dans un ", strong("quartier attractif"), ". Ensuite entrez les informations suivantes :"),
          p(tags$a(href = "https://www.leboncoin.fr/recherche?category=9&locations=Longwy__49.515961_5.764206_6597_5000&real_estate_type=2&immo_sell_type=old,new,viager&energy_rate=e,d,c,b,a&square=9-max&sort=price&order=asc", target = "_blank", class = "btn btn-primary btn-lg", "Ouvrir Leboncoin")),
          
          hr(),
          numericInput("area", "Surface (m²)", value = 50, min = 0, step = 1),
          numericInput("price", "Prix (€)", value = 75000, min = 0, step = 1000),
          textInput("location", "Localisation", value = "Paris"),
          textInput("adLink", "Lien vers l'annonce", value = "https://www.leboncoin.fr/..."),
          actionButton("submit", "Analyser le prix au m²", style = "color: black; background-color: white;"),
          
          hr(),
          h3("Étape 2 : Comparez le avec le marché !"),
          p("Après consultation sur SeLoger.com, entrez les données pour une ", strong("comparaison de marché"), ". Ensuite entrez les informations suivantes : "),
          p(tags$a(href = "https://www.seloger.com/estimation-immobiliere/online?m=homepage-tab-estimer", target = "_blank", class = "btn btn-primary btn-lg", "Ouvrir SeLoger.com")),
          
          hr(),
          numericInput("netSellerPrice", "Prix net vendeur (€)", value = 70000, min = 0, step = 1000),
          numericInput("rentExclCharges", "Loyer hors charges (€/mois)", value = 800, min = 0, step = 10),
          actionButton("compare", "Comparer les données", style = "color: black; background-color: white;"),
          
          hr(),
          h3("Étape 3 : Négociez votre bien !"),
          p("Déterminez le ", strong("prix de négociation"), " pour l'achat.  En janvier 2025, en France, les ", strong("logements classés G"), " (les plus énergivores) ne pourront plus être mis en location. À partir de 2028, cette interdiction sera étendue aux ", strong("logements classés F"), ", et en 2034, aux ", strong("logements classés E"), ". Les résultats seront automatiquement copiés dans le presse-papier. Vous pouvez les coller dans le dataset mis à disposition si vous voulez les garder."),
          actionButton("calculateTotalCost", "Calculer le coût total", style = "color: black; background-color: white;"),
          actionButton("calculateReturn", "Estimer la rentabilité", style = "color: black; background-color: white;"),
          actionButton("calculateNegotiationPrice", "Etape de négociation", style = "color: black; background-color: white;"),
          actionButton("showData", "Afficher/Copier Dataset", style = "color: black; background-color: white;"), 
          actionButton("reset1", "Réinitialiser résultats"),
          
          br(),
          br(),
          downloadButton("downloadDataTxt", "Télécharger Infos", class = "btn-primary btn-lg"),
          
          hr(),
          h3("Étape 4 : Rédigez l'offre d'achat !"),
          p("Rédigez et transmettez l'", strong("offre d'achat"), " au vendeur, puis à votre notaire qui pourra organiser le ", strong("compromis de vente"), " contenant les clauses suspensives."),
          p(tags$a(href = "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fedito.seloger.com%2Fsites%2Fdefault%2Ffiles%2Fedito_migrate%2Farticle%2Fimage%2Flettre_offre_achat_immobilier_prix_inferieur_annonce.docx&wdOrigin=BROWSELINK", target = "_blank", class = "btn btn-primary btn-lg", "Accéder au modèle")),
          
          hr(),
          h3("Étape 5 : Simuler vos travaux et vos aides !"),
          p("Lorsque vous prévoyez d'", strong("améliorer la performance énergétique"), " d'un appartement classé entre D et G au DPE, il est crucial d'adopter une démarche par étapes, avec un ", strong("diagnostic énergétique"), " après chaque intervention majeure, pour s'assurer de l'efficacité des travaux et de la conformité aux futures réglementations sur la location."),
          p("Commencez par ", strong("l'isolation des murs"), ", suivi du remplacement des ", strong("fenêtres et des portes"), " pour réduire les pertes thermiques. Poursuivez avec la ", strong("modernisation du système de chauffage"), " et l'installation d'une ventilation mécanique contrôlée."),
          tags$a(href = "https://www.youtube.com/watch?v=qDESFQ7QtGk", target = "_blank", class = "btn btn-primary btn-lg", "Guide travaux"),
          tags$a(href = "https://www.youtube.com/watch?v=BTjT4_i3qfw", target = "_blank", class = "btn btn-primary btn-lg", "Tutoriel pour DPE"),
          br(),
          br(),
          tags$a(href = "https://particulier.gorenove.fr/", target = "_blank", class = "btn btn-primary btn-lg", "Informations DPE"),
          tags$a(href = "https://www.effy.fr/?utm_medium=cpc&utm_source=google&utm_campaign=brand-generique-search-marqueseule&utm_content=141693438160&utm_term=effy&location=9056516&gad_source=1&gclid=CjwKCAiA75itBhA6EiwAkho9e74FOsuiUwfX_Zavsa9Mosdj5eW86sB3R5p8bErksokhqyEKbTUugBoC3-YQAvD_BwE", target = "_blank", class = "btn btn-primary btn-lg", "Simuler travaux/aides"),
        ),
        ########################################################################
        
        # Utilisation de tabsetPanel pour créer des onglets
        tabPanel(
          "Étude de crédit",
          
          # Contenu de l'onglet "Conseils et prêts avantageux"
          h2("Conseils"),
          p(strong("Privilégie un CDI"), " et une stabilité professionnelle pour rassurer les banques sur la régularité de vos revenus."),
          p(strong("Gère bien tes finances"), " : évitez les découverts bancaires et assurez une gestion saine pour renforcer votre crédibilité."),
          p(strong("Apport personnel"), " : idéalement, constituez un apport de 10 à 20 % du prix du bien pour montrer votre capacité d'épargne."),
          p(strong("Taux d’endettement"), " : maintenez votre taux d'endettement en dessous de 33 %, un critère clé pour les banques."),
          p(strong("Prépare un dossier complet"), " : incluez vos fiches de paie, avis d'imposition, relevés bancaires et tout autre document pertinent."),
          p(strong("Délégation d'assurance"), " : choisissez une assurance emprunteur externe pour réduire le coût total de votre prêt."),
          p("N'oubliez pas de ", strong("comparer les offres de différentes banques"), " afin d'obtenir le meilleur taux pour votre prêt immobilier classique."),
          hr(),
          h3("Prêts avantageux"),
          
          h4("Prêts sans travaux :"),
          p(strong("Prêt étudiant"), ": Prêt dédié aux étudiants, mais pourrait également utilisable pour un achat immobilier avec un différé de remboursement sur 10 ans maximum."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F33827", target = "_blank", "En savoir plus sur le prêt étudiant")),
          
          p(strong("Prêt Accession Action Logement"), ": Prêt à 1 % pour l'achat de la résidence principale, souvent en complément d'un prêt principal."),
          p(a(href = "https://www.actionlogement.fr/pret-accession", target = "_blank", "En savoir plus sur le prêt Action Logement")),
          
          p(strong("PEL (Plan Épargne Logement)"), ": Une épargne à taux avantageux qui donne droit à un prêt après 4 ans de souscription."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F2365", target = "_blank", "En savoir plus sur le PEL")),
          
          p(strong("PTZ (Prêt à Taux Zéro)"), ": Prêt sans intérêts pour les primo-accédants, couvrant jusqu'à 40 % du prix d'un bien neuf."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F10871", target = "_blank", "En savoir plus sur le PTZ")),
          
          p(strong("PAS (Prêt Accession Sociale)"), ": Prêt à taux réduit pour les ménages modestes, sous conditions de ressources."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F221", target = "_blank", "En savoir plus sur le PAS")),
          
          h4("Prêts avec travaux :"),
          p(strong("Éco-PTZ (Prêt à Taux Zéro écologique)"), ": Financement sans intérêts pour réaliser des travaux de rénovation énergétique."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F13234", target = "_blank", "En savoir plus sur l'Éco-PTZ")),
          
          p(strong("PTZ avec travaux"), ": Prêt sans intérêts pour financer à la fois l'achat immobilier et des travaux de rénovation."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F10871", target = "_blank", "En savoir plus sur le PTZ avec travaux")),
          
          p(strong("PAS avec travaux"), ": Prêt à taux réduit pour financer à la fois l'achat et des travaux de rénovation."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F221", target = "_blank", "En savoir plus sur le PAS avec travaux")),
          
          p(strong("Prêt Conventionné avec travaux"), ": Prêt pour financer l'achat et les travaux, ouvrant droit à l'APL."),
          p(a(href = "https://www.service-public.fr/particuliers/vosdroits/F2073", target = "_blank", "En savoir plus sur le Prêt Conventionné")),
          
          hr(),
          
          h3("Simulez votre crédit !"),
          p("Utilisez un simulateur pour estimer les mensualités de votre ", strong("crédit immobilier"), " et pour voir si votre projet est financièrement viable. Vous pouvez ensuite ", strong("calculer votre cashflow"), " dans la section suivante. N'oubliez pas que", strong("les prêts d'entreprises pour les collaborateurs peuvent également être très intéressant !")),
          p(tags$a(href = "https://www.meilleurtaux.com/credit-immobilier/simulation-de-pret-immobilier/calcul-des-mensualites.html", target = "_blank", class = "btn btn-primary btn-lg", "Simuler un crédit")),
          p(tags$a(href = "https://www.meilleurtaux.com/demande-simulation/assurance-de-pret/", target = "_blank", class = "btn btn-primary btn-lg", "Assurer le crédit")),
          p(tags$a(href = "https://www.meilleurtaux.com/comparateur-assurance/assurance-habitation/profil/proprietaire/assurance-pno.html", target = "_blank", class = "btn btn-primary btn-lg", "Simuler PNO")),
          #https://www.meilleurtaux.com/comparateur-assurance/assurance-habitation/profil/proprietaire/assurance-pno.html
          p("Note : Plus la durée du prêt est longue, plus les mensualités seront faibles. Toutefois, cela augmente également le coût total du prêt. Si les taux d'intérêt sont faibles, il y a plus de chances que votre ", strong("bien immobilier s'autofinance"), ". Cela signifie que les ", strong("revenus locatifs couvriront les mensualités du prêt ainsi que les autres dépenses"), ""),
          p("La durée maximale pour un prêt immobilier en France est de ", strong("27 ans"), "")
        ),
        
        
        
        ########################################################################
        
        tabPanel(
          "Calcul du cashflow",
          br(),
          textInput("nom_bien", "Nom du bien", value = "MonBien"),
          hr(),
          
          h3("Les entrées mensuelles"),
          numericInput("loyer_potentiel_hors_charge", strong("Loyer potentiel hors charge"), value = 800, min = 0),
          numericInput("charges_demandees_au_locataire", strong("Charges demandées au locataire"), value = 50, min = 0),
          
          hr(),
          h3("Les sorties non déductibles"),
          numericInput("remboursement_capital", strong("Remboursement de capital"), value = 400, min = 0),
          numericInput("frais_de_notaire", strong("Frais de notaire (environ 8% du bien)"), value = 50, min = 0),
          numericInput("frais_et_hypotheque", strong("Frais et hypothèque"), value = 0, min = 0),
          
          hr(),
          h3("Les sorties déductibles mensuelles"),
          numericInput("interets_emprunt", strong("Intérêts d'emprunt (environ 4% du capital emprunté"), value = 40, min = 0),
          numericInput("assurance_credit", strong("Assurance crédit"), value = 20, min = 0),
          numericInput("taxe_fonciere", strong("Taxe foncière"), value = 70, min = 0),
          numericInput("abonnement_electricite_gaz", strong("Abonnement Électricité/Gaz"), value = 0, min = 0),
          numericInput("charges_copropriete", strong("Charges de copropriété"), value = 0, min = 0),
          numericInput("assurance_PNO", strong("Assurance PNO"), value = 25, min = 0),
          numericInput("abonnement_internet_TV", strong("Abonnement Internet/TV"), value = 0, min = 0),
          numericInput("cotisation_fonciere_entreprise", strong("Cotisation Foncière des Entreprises"), value = 0, min = 0),
          numericInput("frais_comptable", strong("Frais comptable"), value = 0, min = 0),
          numericInput("autres_frais", strong("Autres frais"), value = 0, min = 0),
          numericInput("travaux", strong("Travaux (prévoir environ 5% du bien sur l'année)"), value = 0, min = 0),
          numericInput("amortissement", strong("Amortissement"), value = 0, min = 0),
          
          hr(),
          h3("L'investissement initial"),
          numericInput("cout_total_investissement", strong("Coût total de l'investissement"), value = 75000, min = 0),
          radioButtons("choix", strong("Choix du régime d'imposition"),
                       choices = list("LMNP réel (régime défiscalisant)" = "1", "Micro-foncier (régime de base)" = "2"),
                       selected = "1"),
          
          actionButton("calculer", "Afficher dataset"),
          actionButton("reset2", "Réinitialiser"),
          
          hr(),
          h3("Inscription en ligne"),
          p("Une fois que vous avez calculé et déterminé le ", strong("régime fiscal"), " le plus adapté pour votre activité LMNP (Loueur Meublé Non Professionnel), l'étape suivante consiste à vous immatriculer auprès de l'INPI. Cette démarche est essentielle pour obtenir un ", strong("numéro SIRET"), ", indispensable pour déclarer vos revenus de location meublée."),
          tags$a(href = "https://www.youtube.com/watch?v=4cZLmo-z94k", 
                 target = "_blank", 
                 class = "btn btn-primary btn-lg", 
                 "Tutoriel LMNP"),
          
          hr(),
          h3("Comptabilité annuelle du bien"),
          tags$p("Ce tableau comptable vous permet de visualiser précisément les ", strong("revenus générés"), " par votre investissement immobilier. Un ", strong("cashflow positif"), " signifie que vous n'êtes pas endetté, ce qui est un excellent indicateur pour répéter l'opération avec un autre investissement et vous créer du patrimoine. Ce fichier peut être transmis à votre banque pour démontrer votre stabilité financière et justifier un nouveau prêt immobilier avec un taux d'endettement de 0%."),
          
          downloadButton("telechargerExcel", "Télécharger Dataset", class = "btn btn-primary btn-lg"),
          
          br(),
          br(),
          tags$p("À partir d'un certain nombre d'investissements ou si vos revenus locatifs dépassent les limites du statut LMNP (23 000 € de loyer annuel ET 50 % de vos revenus globaux), il devient pertinent de ", strong("déléguer la gestion à une agence immobilière"), ", même si cela engendre des frais supplémentaires. Cela vous permet de gérer plus efficacement vos investissements sans les rendre ingérables et d'être conseillé pour changer de statut.")
        ), # Fin de premier tabPanel
        
        ##########################################################################
        
        # Section Exemple de contrat
        # Section Exemple de contrat
        tabPanel(
          "Création de contrat",
          
          div(
            class = "contract-creation-section",
            
            # Étape 1 : Comment reconnaître un bon locataire
            h3("Étape 1 : Comment reconnaître un bon locataire ?", class = "section-heading"),
            
            # Introduction
            p("Un bon locataire est celui qui respecte ses engagements, paie son loyer à temps, et prend soin du logement. Voici les principaux critères à vérifier, accompagnés de chiffres indicatifs pour garantir sa solvabilité :"),
            
            # Critères financiers
            h4("1. Solvabilité financière"),
            p("Le locataire doit avoir des revenus stables et suffisants pour payer le loyer. Voici les indicateurs financiers à suivre :"),
            tags$ul(
              tags$li("Revenu mensuel net : Le revenu du locataire doit être ", tags$strong("au moins 3 fois supérieur au loyer"), ". Par exemple, pour un loyer de ", tags$strong("800 €"), ", le locataire doit gagner au moins ", tags$strong("2 400 € net par mois"), ""),
              tags$li("Capacité d'endettement : Le locataire ne doit pas dépasser ", tags$strong("33 % d'endettement"), ". Cela inclut tous ses crédits (immobiliers, consommation, etc.)."),
              tags$li("Garants : Si le locataire ne répond pas aux critères, un garant est nécessaire. Le garant doit avoir un revenu supérieur à ", tags$strong("3 fois le loyer"), "")
            ),
            
            # Stabilité professionnelle
            h4("2. Stabilité professionnelle"),
            p("La stabilité de l'emploi est un excellent indicateur de la capacité du locataire à payer le loyer sur la durée."),
            tags$ul(
              tags$li("Contrat de travail : Privilégiez les locataires en ", tags$strong("CDI"), " ou avec au moins ", tags$strong("2 ans d'ancienneté"), " dans leur poste. Pour les CDD ou les indépendants, demandez des justificatifs financiers supplémentaires."),
              tags$li("Ancienneté : Un locataire en poste depuis ", tags$strong("plus de 2 ans"), " est un signe de stabilité.")
            ),
            
            # Historique locatif
            h4("3. Historique locatif"),
            p("Vérifiez l'historique de paiement du locataire et demandez des références si possible."),
            tags$ul(
              tags$li("Quittances de loyer : Demandez les ", tags$strong("3 dernières quittances de loyer"), " pour vérifier que les paiements sont réguliers."),
              tags$li("Références de l’ancien propriétaire : Une ", tags$strong("lettre de recommandation"), " de l'ancien propriétaire est un plus pour évaluer le comportement du locataire.")
            ),
            
            # Respect des obligations
            h4("4. Respect des obligations"),
            p("Un bon locataire respecte le logement et le voisinage. Voici ce que vous pouvez vérifier :"),
            tags$ul(
              tags$li("Entretien : Un locataire qui prend soin de ses précédents logements est souvent un locataire fiable."),
              tags$li("Respect du voisinage : Renseignez-vous si possible auprès des anciens voisins ou copropriétaires.")
            ),
            
            hr(),
            
            # Étape 2 : Documents à demander
            h3("Étape 2 : Documents à demander pour garantir la solvabilité d'un locataire", class = "section-heading"),
            
            p("Demandez ces documents pour vérifier la solvabilité et la fiabilité du locataire :"),
            
            # Documents financiers
            h4("1. Documents financiers"),
            tags$ul(
              tags$li(tags$strong("Les 3 derniers bulletins de salaire"), " (ou justificatifs pour les indépendants)."),
              tags$li(tags$strong("Le dernier avis d'imposition"), " pour vérifier les revenus annuels."),
              tags$li(tags$strong("Les 3 derniers relevés bancaires"), " pour vérifier la gestion financière.")
            ),
            
            # Documents professionnels
            h4("2. Documents relatifs à la situation professionnelle"),
            tags$ul(
              tags$li(tags$strong("Contrat de travail"), " ou attestation de l'employeur pour confirmer la nature de l'emploi (CDI, CDD, indépendant, etc.)."),
              tags$li(tags$strong("Certificat de l’employeur"), " mentionnant la date d'entrée et la stabilité professionnelle.")
            ),
            
            # Documents d'identité
            h4("3. Documents d'identité"),
            tags$ul(
              tags$li(tags$strong("Copie de la carte d'identité"), " ou du passeport."),
              tags$li(tags$strong("Justificatif de domicile récent"), " (facture d’électricité, eau, quittance de loyer).")
            ),
            
            # Historique locatif
            h4("4. Historique locatif"),
            tags$ul(
              tags$li(tags$strong("Quittances de loyer des 3 derniers mois"), ""),
              tags$li(tags$strong("Lettre de recommandation"), " de l'ancien propriétaire (si possible).")
            ),
            
            # Garant
            h4("5. Documents du garant (si applicable)"),
            p("Si le locataire a besoin d’un garant, demandez les mêmes documents que ceux exigés du locataire :"),
            tags$ul(
              tags$li("Les ", tags$strong("3 derniers bulletins de salaire"), " du garant."),
              tags$li(tags$strong("Le dernier avis d'imposition"), " du garant."),
              tags$li("Une ", tags$strong("copie de la carte d’identité"), " du garant.")
            ),
            
            hr(),
            
            # Étape 3 : Rédiger un contrat
            h3("Étape 3 : Rédiger un Contrat de Location LMNP", class = "section-heading"),
            p("Une fois que vous avez trouvé un locataire qui répond à vos critères, vous pouvez rédiger le contrat. Cliquez ci-dessous pour accéder à un modèle de contrat adapté :"),
            p(tags$a(href = "https://www.bailfacile.fr/contrats/bail-meuble#7-quelles-sont-les-regles-de-fonctionnemen", target = "_blank", class = "btn btn-primary btn-lg", "Modèle de contrat")),
            
            hr(),
            
            # Garantie locative
            h3("Étape 4 : Garantie Locative Visale", class = "section-heading"),
            p("Pour sécuriser vos loyers, utilisez la garantie locative Visale. Cette garantie gratuite couvre les loyers impayés. Cependant, le locataire doit avoir des revenus supérieurs à ", tags$strong("3 fois le loyer hors charges"), ""),
            p(tags$a(href = "https://www.visale.fr/", target = "_blank", class = "btn btn-primary btn-lg", "Découvrir Visale")),
            
            hr(),
            
            # Ressources supplémentaires
            p("Pour en savoir plus sur les documents légaux et la gestion locative, consultez : ", 
              tags$a(href = "https://www.service-public.fr/particuliers/vosdroits/F1169", target = "_blank", "Service Public"), "")
          )
        )
        
      )), # Fin de sidebarPanel
    
    #urlG <- 'https://www.visale.fr/'
    
    mainPanel(
      br(),
      h2("Étude de marché"),
      textOutput("pricePerSqM"),
      textOutput("marketComparison"),
      textOutput("totalCost"),
      textOutput("potentialReturn"),
      br(),
      dataTableOutput("dataTable"),
      uiOutput("negotiationPrice"),
      h2("Calcul du cashflow"),
      
      textOutput("titre"),
      textOutput("rien"),
      tableOutput("resultatTable"),
      textOutput("type"),
      textOutput("cashflow"),
      textOutput("rentabilite"),
      br(),
      br(),
      #texteOutput("lien")
      
      # Affichage des résultats
    )
  ) # Fin de mainPanel
) # Fin de sidebarLayout
# Fin de fluidPage

options(shinyapps.locale.cache = FALSE)

server <- function(input, output, session) {
  
  ##############################################################################
  # Installe le package si nécessaire
  # install.packages("writexl")
  
  library(writexl)
  
  library(openxlsx)
  
  output$telechargerExcel <- downloadHandler(
    filename = function() {
      # Définir le type de régime en fonction du choix
      regime <- if (input$choix == "1") "LMNP_reel" else "Micro_foncier"
      
      # Utiliser le nom du bien comme nom du fichier avec le type de régime
      paste(input$nom_bien, "_Comptabilité-", regime, "_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      # Générer et afficher le dataset avant de le télécharger
      new_dataset <- creerDatasetCashflow(
        loyer_potentiel_hors_charge = input$loyer_potentiel_hors_charge, 
        charges_demandees_au_locataire = input$charges_demandees_au_locataire, 
        remboursement_capital = input$remboursement_capital, 
        frais_de_notaire = input$frais_de_notaire, 
        frais_et_hypotheque = input$frais_et_hypotheque, 
        interets_emprunt = input$interets_emprunt, 
        assurance_credit = input$assurance_credit, 
        taxe_fonciere = input$taxe_fonciere, 
        abonnement_electricite_gaz = input$abonnement_electricite_gaz, 
        charges_copropriete = input$charges_copropriete, 
        assurance_PNO = input$assurance_PNO, 
        abonnement_internet_TV = input$abonnement_internet_TV, 
        cotisation_fonciere_entreprise = input$cotisation_fonciere_entreprise, 
        frais_comptable = input$frais_comptable, 
        autres_frais = input$autres_frais, 
        travaux = input$travaux, 
        amortissement = input$amortissement, 
        choix = input$choix, 
        cout_total_investissement = input$cout_total_investissement
      )
      
      # Transposer le dataset
      resultat_dataset_transpose(new_dataset)
      
      # Afficher le dataset dans l'interface utilisateur
      output$resultatTable <- renderTable({
        resultat_dataset_transpose()
      }, rownames = TRUE, na = "")
      
      # Vérifier si le dataset est non vide avant de créer le fichier Excel
      result <- resultat_dataset_transpose()
      
      if (nrow(result) > 0) {
        result_with_row_names <- data.frame(Eléments = rownames(result), result)
        
        # Remplacer les NA par des chaînes vides ou des 0
        result_with_row_names[is.na(result_with_row_names)] <- ""
        
        # Créer un fichier Excel avec openxlsx
        wb <- createWorkbook()
        addWorksheet(wb, "Resultats")
        
        # Appliquer des styles pour les nombres et les pourcentages
        moneyStyle <- createStyle(numFmt = "€ #,##0.00")
        percentStyle <- createStyle(numFmt = "0.00%")
        
        # Écrire les données dans la feuille Excel
        writeData(wb, "Resultats", result_with_row_names)
        writeData(wb, "Resultats", result_with_row_names, colWidths = "auto")
        
        numCols <- ncol(result_with_row_names)
        numRows <- nrow(result_with_row_names) + 1
        
        # Sécurité sur les colonnes existantes
        cols_money <- intersect(c(3:21, 23:25), 1:numCols)
        cols_percent <- intersect(c(22, 26), 1:numCols)
        
        addStyle(wb, sheet = "Resultats", style = moneyStyle, rows = 2:numRows, cols = cols_money, gridExpand = TRUE)
        addStyle(wb, sheet = "Resultats", style = percentStyle, rows = 2:numRows, cols = cols_percent, gridExpand = TRUE)
        
        # Nombre de lignes et colonnes du dataset
        numRows <- nrow(result_with_row_names) + 1  # Inclure l'en-tête
        numCols <- ncol(result_with_row_names)
        
        # Appliquer des formats spécifiques aux colonnes (monétaires et pourcentages)
        if (numCols >= 26) {  # Vérifier que les colonnes existent
          addStyle(wb, sheet = "Resultats", style = moneyStyle, rows = 2:numRows, cols = c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25))
          addStyle(wb, sheet = "Resultats", style = percentStyle, rows = 2:numRows, cols = c(22, 26))
        }
        
        # Sauvegarder le fichier Excel dans l'emplacement spécifié
        saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        stop("Les données sont vides. Impossible de télécharger le fichier.")
      }
    }
  )
  
  
  
  observeEvent(input$ouvrirVISA, {
    urlSimulateur <- "https://www.visale.fr/"
    p(tags$a(href = "https://www.visale.fr/", target = "_blank", class = "btn btn-primary btn-lg", "Découvrir Visale"))
    utils::browseURL(urlSimulateur)
  })
  
  observeEvent(input$openSimulateur, {
    urlSimulateur <- "https://www.meilleurtaux.com/credit-immobilier/simulation-de-pret-immobilier/calcul-des-mensualites.html"
    p(tags$a(href = "https://www.meilleurtaux.com/credit-immobilier/simulation-de-pret-immobilier/calcul-des-mensualites.html", target = "_blank", class = "btn btn-primary btn-lg", "Simuler un crédit"))
    utils::browseURL(urlSimulateur)
  })
  
  observeEvent(input$reset1, {
    # Réinitialiser les autres sorties
    output$pricePerSqM <- renderText({ "" })
    output$marketComparison <- renderText({ "" })
    output$totalCost <- renderText({ "" })
    output$potentialReturn <- renderText({ "" })
    output$negotiationPrice <- renderUI({ HTML("") })
    
    # Réinitialiser le dataset
    #output$resultatTable <- renderDataTable({
    # data.frame()  # un dataset vide
    #})
  })
  
  
  observeEvent(input$openLBC, {
    urlLBC <- "https://www.leboncoin.fr/recherche?category=9&locations=Paris__48.85320639974214_2.344542891922454_6603_5000&sort=price&order=asc&real_estate_type=2&immo_sell_type=old%2Cnew&energy_rate=g%2Cf%2Ce&price=min-150000&square=10-max"
    actionLink("openLBC", "Ouvrir Leboncoin", href = "https://www.leboncoin.fr/recherche?category=9&locations=Paris__48.85320639974214_2.344542891922454_6603_5000&sort=relevance&real_estate_type=2&immo_sell_type=old%2Cnew&price=min-150000&square=9-max", target = "_blank")
    utils::browseURL(urlLBC)
  })
  
  observeEvent(input$openSeLoger, {
    urlSeLoger <- "https://www.seloger.com/estimation-immobiliere/online?m=homepage-tab-estimer"
    actionLink("openSeLoger", "Ouvrir SeLoger.com", href = "https://www.seloger.com/estimation-immobiliere/online?m=homepage-tab-estimer", target = "_blank", class = "btn-primary btn-lg")
    utils::browseURL(urlSeLoger)
  })
  
  observeEvent(input$redigerOffre, {
    urlModeleOffre <- "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fedito.seloger.com%2Fsites%2Fdefault%2Ffiles%2Fedito_migrate%2Farticle%2Fimage%2Flettre_offre_achat_immobilier_prix_inferieur_annonce.docx&wdOrigin=BROWSELINK"
    actionLink("redigerOffre", "Accéder au modèle", href = "https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fedito.seloger.com%2Fsites%2Fdefault%2Ffiles%2Fedito_migrate%2Farticle%2Fimage%2Flettre_offre_achat_immobilier_prix_inferieur_annonce.docx&wdOrigin=BROWSELINK", target = "_blank", class = "btn-primary btn-lg")
    utils::browseURL(urlModeleOffre)
  })
  
  output$pricePerSqM <- renderText({
    req(input$submit)
    if (input$area > 0) {
      pricePerSqM <- input$price / input$area
      paste("\n\nPrix par mètre carré de l'annonce :", round(pricePerSqM, 2), "€/m²")
    } else {
      "Veuillez entrer une surface valide."
    }
  })
  
  output$marketComparison <- renderText({
    req(input$compare)
    if (input$netSellerPrice > 0 && input$rentExclCharges > 0) {
      estimatedPricePerSqM <- input$netSellerPrice / input$area
      paste("\nPrix estimé par mètre carré sur SeLoger :", round(estimatedPricePerSqM, 2), "€/m²")
    } else {
      "Veuillez entrer des données de marché valides."
    }
  })
  
  
  output$totalCost <- renderText({
    req(input$calculateTotalCost)
    if (input$price > 0) {
      totalCost <- input$price * 1.08
      paste("Coût total d'achat (avec frais de notaire d'environ 8%) :", round(totalCost, 2), "€")
    } else {
      "Veuillez entrer un prix valide."
    }
  })
  
  
  output$potentialReturn <- renderText({
    req(input$calculateReturn)  # Ensure that the button has been clicked
    totalCost <- input$price * 1.08  # Calculate total cost
    
    if (totalCost > 0 && input$rentExclCharges > 0) {
      potentialReturn <- (input$rentExclCharges * 12 / totalCost) * 100
      paste("Rentabilité potentielle brute estimé :", round(potentialReturn, 2), "%")
    } else {
      "Veuillez entrer un prix d'achat et un loyer valides."
    }
  })
  
  output$potentialReturn <- renderText({
    req(input$calculateReturn)  # Ensure that the button has been clicked
    totalCost <- input$price * 1.08  # Calculate total cost
    
    if (totalCost > 0 && input$rentExclCharges > 0) {
      potentialReturn <- (input$rentExclCharges * 12 / totalCost) * 100
      paste("Rentabilité potentielle brute estimé:", round(potentialReturn, 2), "%")
    } else {
      "Veuillez entrer un prix d'achat et un loyer valides."
    }
  })
  
  observeEvent(input$calculateNegotiationPrice, {
    if (input$netSellerPrice < input$price || (input$rentExclCharges * 12 / (input$price * 1.08)) < 0.08) {
      prixNego1 <- input$netSellerPrice
      prixNego2 <- input$rentExclCharges * 12 / (0.08 * 1.08)
      prixNego <- min(prixNego1, prixNego2)
    }
    else {
      prixNego <- input$price * 0.95
    }
    
    premierPrixPropose <- prixNego * 0.75
    deuxiemePrixPropose <- prixNego * 0.85
    troisiemePrixPropose <- prixNego * 0.95
    
    output$negotiationPrice <- renderUI({
      HTML(paste(
        "<br><b>Objectif : obtenir un prix de vente en dessous du prix du marché pour la revente et une rentabilité d'au moins 8% brut (bonne rentabilité) :</b>",
        "<br><br>Prix à négocier : ", round(prixNego, 2), "€",
        "<br><b>Première proposition de négociation (25% en dessous de l'estimation) :</b> Bonjour, je suis intéressé par votre annonce pour le bien. Toutefois, en considérant ses défauts tels que la classe énergétique, l'emplacement, l'usure, etc., je pense qu'il est actuellement surévalué. Suite à mon analyse, ma première offre est de ", round(premierPrixPropose, 2), "€.",
        "<br><b>Deuxième proposition de négociation (15% en dessous de l'estimation) :</b> Bonjour, après réflexion, je souhaite ajuster notre proposition pour le bien à ", round(deuxiemePrixPropose, 2), "€.",
        "<br><b>Troisième proposition de négociation (5% en dessous de l'estimation) :</b> Bonjour, pour finaliser notre négociation, je propose mon offre finale de ", round(troisiemePrixPropose, 2), "€ pour l'appartement. Je vous enverrais ensuite mon offre d'achat pour l'appartement.", sep = "\n\n"))
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$nom_annonce, "_Recherche d'appartemment_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("H:/Desktop/Immo Gains/Recherche d'appartemment.xlsx", file)
    }
  )
  
  ##############################################################################
  creerDataset <- reactive({
    # Initialisation des variables avec une valeur par défaut
    prixNego <- NA
    prixNego25 <- NA
    prixNego15 <- NA
    prixNego5 <- NA
    
    if (input$netSellerPrice <= input$price || (input$rentExclCharges * 12 / (input$price * 1.08)) < 0.08) {
      prixNego1 <- input$netSellerPrice
      prixNego2 <- input$rentExclCharges * 12 / (0.08 * 1.08)
      prixNego <- min(prixNego1, prixNego2)
    }
    else {
      prixNego <- input$price * 0.95
    }
    prixNego25 <- prixNego * 0.75
    prixNego15 <- prixNego * 0.85
    prixNego5 <- prixNego * 0.95
    
    prix <- input$price
    loyer <- input$rentExclCharges
    rentabilite <- ((loyer * 12) / (prix * 1.08)) * 100
    lieu <- input$location
    lien <- input$adLink
    
    dataset <- data.frame(
      Prix = round(prix, 2),
      Loyer = round(loyer, 2),
      Rentabilité = round(rentabilite, 2),
      Lieu = lieu,
      Lien = lien,
      "PrixNégo" = round(prixNego, 2),
      "PrixNégo1" = round(prixNego25, 2),
      "PrixNégo2" = round(prixNego15, 2),
      "PrixNégo3" = round(prixNego5, 2)
    )
    
    # Convertir le dataframe en liste de chaînes de caractères
    liste <- as.character(unlist(dataset))
    
    # Créer une chaîne de caractères pour le presse-papiers
    clipboard <- paste(liste, collapse = "\n")
    
    # Copier dans le presse-papiers
    writeClipboard(clipboard)
    
    return(dataset)
  })
  
  
  # Observer pour afficher le dataset
  observeEvent(input$showData, {
    output$dataTable <- renderDataTable({
      creerDataset() # Ceci génère le dataframe et l'imprime déjà
    })
  })
  
  # Initialize resultat_dataset_transpose outside of the observeEvent so it can be accessed anywhere
  resultat_dataset_transpose <- reactiveVal(data.frame())
  
  observeEvent(input$calculer, {
    
    
    
    
    cashflow <- calculCashflow(input$loyer_potentiel_hors_charge, input$charges_demandees_au_locataire, input$remboursement_capital, input$frais_de_notaire, input$frais_et_hypotheque, 
                               input$interets_emprunt, input$assurance_credit, input$taxe_fonciere, input$abonnement_electricite_gaz, 
                               input$charges_copropriete, input$assurance_PNO, input$abonnement_internet_TV, input$cotisation_fonciere_entreprise, 
                               input$frais_comptable, input$autres_frais, input$travaux, input$amortissement, 
                               input$choix)
    
    rentabilite <- calculRentabilite(input$loyer_potentiel_hors_charge, input$charges_demandees_au_locataire, input$remboursement_capital, input$frais_de_notaire, input$frais_et_hypotheque, 
                                     input$interets_emprunt, input$assurance_credit, input$taxe_fonciere, input$abonnement_electricite_gaz, 
                                     input$charges_copropriete, input$assurance_PNO, input$abonnement_internet_TV, input$cotisation_fonciere_entreprise, 
                                     input$frais_comptable, input$autres_frais, input$travaux, input$amortissement, 
                                     input$choix, input$cout_total_investissement)
    
    
    
    output$titre <- renderText(paste("Nous avons préparé un ensemble de données détaillé pour vous aider à mieux comprendre votre investissement. Voici un aperçu de ces informations :"))
    output$rien <- renderText(paste("Vous pouvez désormais utiliser ce dataset comptable,"))
    output$type <- output$type <- renderText(paste("Sur la base de vos entrées, le régime fiscal pour votre investissement est :", regimeFiscal(input$choix)))
    output$cashflow <- renderText(paste("Après une analyse détaillée de vos coûts et revenus, nous avons calculé que votre cashflow mensuel s'élève à :", cashflow, "euros."))
    output$rentabilite <- renderText(paste("En prenant en compte tous les facteurs, la rentabilité net finale de votre investissement est estimée à :", round(rentabilite*100, 2), "% par an."))
    
    
    # Création du dataset avec les valeurs calculées
    new_dataset <- creerDatasetCashflow(
      loyer_potentiel_hors_charge = input$loyer_potentiel_hors_charge, 
      charges_demandees_au_locataire = input$charges_demandees_au_locataire, 
      remboursement_capital = input$remboursement_capital, 
      frais_de_notaire = input$frais_de_notaire, 
      frais_et_hypotheque = input$frais_et_hypotheque, 
      interets_emprunt = input$interets_emprunt, 
      assurance_credit = input$assurance_credit, 
      taxe_fonciere = input$taxe_fonciere, 
      abonnement_electricite_gaz = input$abonnement_electricite_gaz, 
      charges_copropriete = input$charges_copropriete, 
      assurance_PNO = input$assurance_PNO, 
      abonnement_internet_TV = input$abonnement_internet_TV, 
      cotisation_fonciere_entreprise = input$cotisation_fonciere_entreprise, 
      frais_comptable = input$frais_comptable, 
      autres_frais = input$autres_frais, 
      travaux = input$travaux, 
      amortissement = input$amortissement, 
      choix = input$choix, 
      cout_total_investissement = input$cout_total_investissement
    )
    
    # Transposition du dataset
    resultat_dataset_transpose(new_dataset)
    # Affiche le contenu du dataset
    #print(resultat_dataset_transpose())
    shinyjs::enable("telechargerExcel")
    
  })
  
  
  
  observeEvent(input$reset2, {
    
    
    output$titre <- renderText(paste(""))
    output$rien <- renderText(paste(""))
    output$type <- renderText(paste(""))
    output$cashflow <- renderText(paste(""))
    output$rentabilite <- renderText(paste(""))
    
    reset_values <- list(
      loyer_potentiel_hors_charge = 2000, 
      charges_demandees_au_locataire = 0, 
      remboursement_capital = 500, 
      frais_de_notaire = 0, 
      frais_et_hypotheque = 0, 
      interets_emprunt = 500, 
      assurance_credit = 0, 
      taxe_fonciere = 0, 
      abonnement_electricite_gaz = 0, 
      charges_copropriete = 0, 
      assurance_PNO = 0, 
      abonnement_internet_TV = 0, 
      cotisation_fonciere_entreprise = 0, 
      frais_comptable = 0, 
      autres_frais = 0, 
      travaux = 0, 
      amortissement = 0, 
      choix = "1", 
      cout_total_investissement = 100000
    )
    # Update each input value
    lapply(names(reset_values), function(name) {
      updateNumericInput(session, name, value = reset_values[[name]])
    })
    updateRadioButtons(session, "choix", selected = "1")
    
    # Clear the table
    resultat_dataset_transpose(data.frame())
  })
  
  output$resultatTable <- renderTable({
    resultat_dataset_transpose()
  }, rownames = TRUE, na = "")
  
  
  observeEvent(input$ouvrirContrat, {
    ouvrirContract()
  })
  
  
  # Fonction réactive pour calculer le prix par mètre carré
  price_per_sq_m <- reactive({
    req(input$area, input$price)  # Assure que les champs sont renseignés
    if (input$area > 0) {
      return(input$price / input$area)  # Calcule le prix par m²
    } else {
      return(NA)
    }
  })
  
  # Fonction réactive pour comparer avec le marché
  market_comparison <- reactive({
    req(input$netSellerPrice, input$area)
    if (input$netSellerPrice > 0 && input$area > 0) {
      return(input$netSellerPrice / input$area)  # Compare avec le prix moyen du marché
    } else {
      return(NA)
    }
  })
  
  # Fonction réactive pour calculer le coût total d'achat (avec frais de notaire)
  total_cost <- reactive({
    req(input$price)
    if (input$price > 0) {
      return(input$price * 1.08)  # Ajoute 8% pour les frais de notaire
    } else {
      return(NA)
    }
  })
  
  # Fonction réactive pour calculer la rentabilité brute
  potential_return <- reactive({
    req(input$rentExclCharges, total_cost())
    if (total_cost() > 0 && input$rentExclCharges > 0) {
      return((input$rentExclCharges * 12 / total_cost()) * 100)  # Rentabilité brute sur un an
    } else {
      return(NA)
    }
  })
  
  # Fonction réactive pour calculer le prix à négocier
  negotiation_price <- reactive({
    req(input$netSellerPrice, input$price, input$rentExclCharges)
    if (input$netSellerPrice <= input$price || (input$rentExclCharges * 12 / (input$price * 1.08)) < 0.08) {
      prixNego1 <- input$netSellerPrice
      prixNego2 <- input$rentExclCharges * 12 / (0.08 * 1.08)
      return(min(prixNego1, prixNego2))  # Le minimum entre les deux
    } else {
      return(input$price * 0.95)  # 5% en dessous du prix actuel si tout semble correct
    }
  })
  
  # Fonction de téléchargement du fichier texte
  output$downloadDataTxt <- downloadHandler(
    filename = function() {
      # Utiliser le nom de l'annonce entré par l'utilisateur pour nommer le fichier
      paste(input$nom_annonce, "_Etude-de-bien_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Récupérer les valeurs des calculs
      price_per_sq_m_value <- price_per_sq_m()
      market_comparison_value <- market_comparison()
      total_cost_value <- total_cost()
      potential_return_value <- potential_return()
      negotiation_price_value <- negotiation_price()
      
      # Lien vers l'annonce
      ad_link <- input$adLink
      
      # Calcul des propositions de négociation (25%, 15%, 5% en dessous du prix négocié)
      premierPrixPropose <- negotiation_price_value * 0.75
      deuxiemePrixPropose <- negotiation_price_value * 0.85
      troisiemePrixPropose <- negotiation_price_value * 0.95
      
      # Construire le contenu du fichier de manière claire et professionnelle
      content <- paste(
        "Étude de marché pour le bien immobilier : ", ad_link,
        "\n\nPrix par mètre carré demandé (en €/m²) : ", round(price_per_sq_m_value, 2),
        "\nComparaison avec le marché (en €/m²) : ", round(market_comparison_value, 2),
        "\nCoût total estimé (incluant frais de notaire, en €) : ", round(total_cost_value, 2),
        "\nRentabilité brute estimée (en %) : ", round(potential_return_value, 2),
        
        "\n\n### PROPOSITIONS DE NÉGOCIATION ###",
        "\nObjectif : **Négocier un prix final autour de ", round(negotiation_price_value, 2), "€** pour atteindre une rentabilité brute d'au moins 8%.",
        
        "\n\nPremière proposition de négociation (25% en dessous de l'estimation) : Bonjour, je suis intéressé par votre annonce. Toutefois, en considérant ses défauts tels que la classe énergétique, l'emplacement, l'usure, etc., je pense qu'il est actuellement surévalué. Suite à mon analyse, ma première offre est de ", round(premierPrixPropose, 2), "€.",
        
        "\n\nDeuxième proposition de négociation (15% en dessous) : Bonjour, après réflexion, je souhaite ajuster notre proposition à ", round(deuxiemePrixPropose, 2), "€.",
        
        "\n\nTroisième proposition de négociation (5% en dessous) : Bonjour, pour finaliser notre négociation, je propose mon offre finale de ", round(troisiemePrixPropose, 2), "€. Je vous enverrai ensuite mon offre d'achat pour l'appartement.",
        
        sep = "\n"
      )
      
      # Écrire le contenu dans le fichier texte
      write(content, file)
    }
  )
  
  output$generated_prompt <- renderText({
    city <- input$user_city
    
    if (nchar(city) > 0) {
      paste0("Tu es un expert en investissement. Mon objectif est d'investir dans l'immobilier à moins d'une heure de ", city, 
             " (inclus). Mon objectif est de trouver des biens offrant une rentabilité brute supérieure à 8 %. ",
             "La ville cible doit posséder une gare, avoir plus de 20 000 habitants (de préférence), avoir une croissance démographique positive, ",
             "un faible taux de chômage, et une forte demande locative (plus de 60 % de locataires). ",
             "Je m'intéresse particulièrement aux studios et T2 dans des quartiers en développement. ",
             "La rentabilité locative brute est calculée avec la formule suivante : ((Loyer mensuel brut x 12) / Prix d'achat) x 100. ",
             "Merci de fournir un Top 10 des villes répondant à ces critères, avec la rentabilité brute médiane estimée pour le lieu, le prix au m2, la population, la qualité des locataires, la distance, ",
             "l'évolution du marché et des suggestions d'investissement etc.. Dis la vérité, soit le plus réaliste possible et trie par rendement.")
    } else {
      "Merci d'indiquer une ville de référence pour votre recherche."
    }
  })
  
  
}

shinyApp(ui = ui, server = server)
