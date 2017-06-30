#!/usr/bin/env groovy

node('uk_centos6_cluster') {

    env.PATH = "/illumina/thirdparty/R/R-3.2.3/bin:${env.PATH}"
    env._R_CHECK_FORCE_SUGGESTS_ = false

    // centos 6.5 compiled pandoc binary
    env.RSTUDIO_PANDOC = '/illumina/development/curium/bin'
    
    // 3.2.3 rlibs
    env.R_LIBS_USER = '/illumina/thirdparty/bmoore1/rlibs-3.2.3'
    
    stage('Checkout') {
        checkout scm
    }

    stage('Build') {
        sh "R CMD build ."
    }

    stage('Test') {
        sh "R CMD check happyR_*.tar.gz"
    }
    
    stage('Test coverage') {
        sh "Rscript -e 'covr::report(covr::package_coverage(), file=\"/illumina/development/www/python/codecov/static/happyR.html\", browse = F)'"
    }

    stage('Cleanup') {
        deleteDir()
    }

}
