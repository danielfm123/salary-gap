options("h2o.use.data.table" = TRUE)
options(stringsAsFactors = FALSE)
options(java.parameters = "-Xmx4096m")
options(dplyr.width = Inf) 
options(dplyr.print_max = 100) 
Sys.setenv(TZ='GMT')

if(version$os == "mingw32"){
  # Hacer si es Windows
}else{
  #Cosas que pasan cuando corre en linux
}

librerias_a_cargar = c("tidyverse",
                       "future",
                       "compiler",
                       "readr")
for(lib in librerias_a_cargar){
  # print(lib)
  if(!lib %in% rownames(installed.packages())){
    print(paste("installing",lib))
    install.packages(lib)
  }
  library(package = lib, character.only = T)
}
plan(multiprocess)


multiple_opt = c("DevType",
                 "CommunicationTools",
                 "EducationTypes",
                 "SelfTaughtTypes",
                 "HackathonReasons",
                 "LanguageWorkedWith",
                 "LanguageDesireNextYear",
                 "DatabaseWorkedWith",
                 "DatabaseDesireNextYear",
                 "PlatformWorkedWith",
                 "PlatformDesireNextYear",
                 "FrameworkWorkedWith",
                 "FrameworkDesireNextYear",
                 "IDE",
                 "Methodology",
                 "VersionControl",
                 "AdBlockerReasons",
                 "ErgonomicDevices",
                 "Gender",
                 "SexualOrientation",
                 "RaceEthnicity",
                 'AdsActions')

for (f in dir("functions",full.names = T,recursive = T)){
  source(f)
}

