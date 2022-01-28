# How to run test programs

  1. Set up a PostgreSQL server.
  2. Write configuration to embed_db_info.sh.
  3. Run "./basic" to start test programs.




# How to run test programs for Docker Compose

  1. Set up a PostgreSQL server.    
     Place docker-compose.yml and Dockerfile in the same directory.  
     Go to the directory, and run  ```docker-compose up -d ```.
     
     ```
     # docker-compose.yml
     version: '3'

      services:
        db_postgres:
          image: postgres:10
          environment:
            - POSTGRES_USER=main_user
            - POSTGRES_PASSWORD=password
            - POSTGRES_DB=testdb
          ports:
            - "5432"

        ocesql:
          build:
            dockerfile: ./Dockerfile
          stdin_open: true
          tty: true
          depends_on:
            db_postgres:
              condition: service_started
          logging:
            driver: none
     ```
     
     ```
     # Dockerfile
      FROM centos:centos7

      # install yum package
      RUN yum install -y gcc gcc-devel gcc-c++ make bison flex gmp-devel ncurses-devel postgresql-devel postgresql-server autoconf psql

      # install opensource COBOL
      ADD https://github.com/opensourcecobol/opensource-cobol/archive/v1.5.2J.tar.gz opensource-cobol-1.5.2J.tar.gz
      RUN tar zxvf opensource-cobol-1.5.2J.tar.gz &&\
          cd /opensource-cobol-1.5.2J/vbisam &&\
          ./configure --prefix=/usr/ &&\
          make install &&\
          cd /opensource-cobol-1.5.2J &&\
          ./configure --prefix=/usr/ --with-vbisam &&\
          make install &&\
          cd / &&\
          rm -rf opensource-cobol-1.5.2J.tar.gz

      # install OCESQL
      
      ### The ADD instruction copies new files, directories or remote file URLs from <src> and adds them to the filesystem of the image at the path <dest>. 
      ### If you want to run another version or your own test, rewrite it as needed.
      
      ADD https://github.com/opensourcecobol/Open-COBOL-ESQL/archive/refs/heads/develop.tar.gz Open-COBOL-ESQL-develop.tar.gz
      RUN tar zxvf Open-COBOL-ESQL-develop.tar.gz &&\
          cd /Open-COBOL-ESQL-develop &&\
          ./configure --prefix=/usr/ &&\
          make install &&\
          cd / &&\
          rm -rf Open-COBOL-ESQL-1.2.tar.gz

      ENTRYPOINT ["/bin/bash"]

      RUN ls
     
     ```
     
  2. After attaching to the running container(e.g. xxxx_ocesql-1), make sure you can connect to the PostgreSQL container.    
     ```PGPASSWORD=password psql -h db_postgres -U main_user -d testdb```
  
  3. Write configuration to embed_db_info.sh.    
     Copy embed_db_info_docker.sh to embed_db_info.sh. The settings should be the same as docker-compose.yml.
  
  4. Run ```./basic``` to start test programs.
