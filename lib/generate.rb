require 'fileutils'
require 'yaml'
require 'erb'

option_keys = []
Dir::glob("ansible/library/**/*").each {|f|
  if File.directory? f
    FileUtils.mkdir_p(File.join("../snippets/text-mode/ansible", File.basename(f)))
    next
  end
  yml = ''
  start = false
  open(f).each {|line|
    start = false if /^'''/.match line
    start = false if /^"""/.match line
    line.gsub!(/(.+)\- "When(.+)"$/, '\1- When\2') # for postgresql_user DOCUMENTATION
    yml << line if start
    start = true if /^DOCUMENTATION = .?'''/.match line
    start = true if /^DOCUMENTATION = .?"""/.match line
  } if File.file? f
  doc = YAML.load(yml)
  next unless doc
  index = 2
  options = ''
  doc['options'].each {|key, value|
    option_keys << key
    next unless value['required']
    options << ' '
    options << key
    if value['default']
      options << '=${'
      options << index.to_s
      options << ':'
      options << value['default'].to_s
      options << '}'
    else
      options << '=$'
      options << index.to_s
    end
    index += 1
  } if doc['options']
  options = ' $2' unless doc['options']

  options << ' $0'
  template = <<EOS
# name : <%= doc['short_description'] %>
# key : <%= doc['module'] %>
# condition: ansible
# --
- name: ${1:<%= doc['short_description'] %>}
  <%= doc['module'] %>:<%= options %>
EOS
  snippet = ERB.new template
  dirname = File.basename(File.dirname(f))
  File.write(File.join("../snippets/text-mode/ansible", dirname, File.basename(f)), snippet.result(binding))

  File.write("../dict/ansible", option_keys.uniq.join("\n"))
}
