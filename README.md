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
  racket -t webapp.rkt
```
Alternatively, open webapp.rkt using DrRacket and press Run (Ctrl-R)

The app will run on URL localhost:8000/servlets/webapp.rkt

## Login details
For student dashboard uname: `student`
For admin dashboard uname: `admin`
No password required
