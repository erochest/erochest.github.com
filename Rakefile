
task :default => ['ericrochester.rdf']


desc 'This pushes the site back to github.'
task :push do
  system('git push')
end

desc 'This starts jekyll running a server that resets itself when there are changes.'
task :serve do
  system('jekyll --server --auto')
end

desc 'This updates the RDF/XML FOAF file from the Turtle file.'
task 'ericrochester.rdf' => 'ericrochester.ttl' do
  system('rdfcat -out RDF/XML -ttl ericrochester.ttl > ericrochester.rdf')
end

