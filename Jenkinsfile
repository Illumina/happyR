#!/usr/bin/env groovy

node('uk_centos6_cluster') {

    env.PATH = "/illumina/thirdparty/R/R-3.2.3/bin:${env.PATH}"
    env._R_CHECK_FORCE_SUGGESTS_ = false

    // centos 6.5 compiled pandoc binary
    env.RSTUDIO_PANDOC = '/illumina/development/curium/bin'
    
    stage('Checkout') {
        checkout scm
    }

    stage('Build') {
        sh "R CMD build ."
    }

    stage('Test') {
        sh "R CMD check --no-examples happyR_*.tar.gz"
    }

    stage('Document') {
        // pkgdown needs to be run offline and pushed with commit due to dependencies
        sh 'cp -r docs/* /illumina/development/www/python/happyr/static//.'
        sh 'chmod a+w -Rf /illumina/development/www/python/happyr/static/ || true'
        
    }
    
    stage('Cleanup') {
        deleteDir()
    }

}
