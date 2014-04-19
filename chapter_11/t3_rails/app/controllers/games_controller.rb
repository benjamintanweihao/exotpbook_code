require 'socket'

class GamesController < ApplicationController
  before_filter :authenticate_user!

  # Listing all games 
  def index
    url = URI.parse("#{server_url}/games")
    req = Net::HTTP::Get.new(url.to_s)
    res = Net::HTTP.start(url.host, url.port) do |http|
      http.request(req)
    end 
    @games = JSON.parse(res.body)
  end

  # Player X joins a game
  def new
    url = URI.parse("#{server_url}/game?player_id=#{current_user.id}")
    req = Net::HTTP::Post.new(url.to_s)

    res = Net::HTTP.start(url.host, url.port) do |http|
      http.request(req)
    end 

    json = JSON.parse(res.body)

    if json["status"] == "success"
      game_id = json["data"]["game_id"]
      session[:side] = "x"
      redirect_to "/games/#{game_id}"
    else
      flash[:error] = "Error creating game: #{json["message"]}"
      redirect_to :back
    end
  end

  # Player O joins a game
  def join
    game_id = params[:game_id]
    url = URI.parse("#{server_url}/games/#{game_id}/join?player_id=#{current_user.id}")
    req = Net::HTTP::Post.new(url.to_s)
    res = Net::HTTP.start(url.host, url.port) do |http|
      http.request(req)
    end 
    json = JSON.parse(res.body)

    if json["status"] == "success"
      game_id = json["data"]["game_id"]
      session[:side] = "o"
      redirect_to "/games/#{game_id}"
    else
      flash[:error] = "Error joining game: #{json["message"]}"
      redirect_to :back
    end
  end 

private
  def server_url
    ip = Socket.ip_address_list.detect { |intf| intf.ipv4_private? }
    "http://#{ip.ip_address}:4000"
  end

end
