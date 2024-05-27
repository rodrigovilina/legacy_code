# frozen_string_literal: true

class App
  def call(_env)
    headers = {
      'Content-Type' => 'text/html'
    }

    response = ['<h1>Hello World!</h1>']

    [200, headers, response]
  end
end
