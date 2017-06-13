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
    
    stage('Test coverage') {
        sh "Rscript -e 'covr::report(covr::package_coverage(), file=\"/illumina/development/www/python/codecov/static/happyR.html\", browse = F)'"
    }

    stage('Cleanup') {
        deleteDir()
    }

}
