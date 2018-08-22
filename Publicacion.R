#=================    PUBLICACION EN SHINY SERVER    =======================
getwd()
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App",
                     account = "cristianpachacama",
                     appName = "ClusterIPC")
