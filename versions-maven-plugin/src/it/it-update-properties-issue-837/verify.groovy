assert ! ( new File( basedir, "build.log" ).text.contains( 'Property ${artifact-version}' ) )
