T3Rails::Application.routes.draw do
  devise_for :users

  resources :games do
    get 'join'
  end
  root to: "home#index"
end
