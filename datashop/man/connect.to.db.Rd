\name{connect.to.db}
\alias{connect.to.db}
\title{Connect to a database}
\usage{
  connect.to.db(dbname, host = "pslc-db",
    usrpass = "dbreader", set.default.connection = TRUE)
}
\arguments{
  \item{dbname}{the name of the database to connect to}

  \item{host}{the hostname of the database server}

  \item{usrpass}{the username and password to login with}

  \item{set.default.connection}{if true, the global
  variable default.connection will be set}
}
\description{
  This function establishes a connection to a database, and
  returns the connection object. The connection is also set
  as the global variable default.connection
}

