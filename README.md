# Locker Management System

## Installing Racket
In order to run the webapp, Racket must be installed from

https://download.racket-lang.org/

## Installing dependencies
The following collections need to be installed using the following commands

```
  raco pkg install string-util
  raco pkg install csv-reading
```

Alternatively, these can be installed from the Racket IDE, DrRacket, which comes with the installation

If offered to install additional dependencies, follow the given instructions to install them as well

## Downloading code
To download the source code, run the following command

  `git clone https://github.com/TeodorSK/LockerManagementSystem`

## Running
To run the webapp, run the following commands
```
  cd LockerManagementSystem/lms1.0
  racket webapp.rkt
```

The app will run on URL localhost:8005/webapp.rkt

## Login details
The system uses the CAS attribute `activeclasses` to determine user authorization. `staff` and `instructor` classes are given admin privileges, provided in a file containing employee numbers named `auth-admins`, located in the `LockerManagementSystem/lms1.0/htdocs` directory. Student privileges are given to users whose student number is present in the database.
