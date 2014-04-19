module ApplicationHelper
  def random_md5
    Digest::MD5.hexdigest((0...8).map { (65 + rand(26)).chr }.join)
  end

  def identicon_link(md5)
    "http://www.gravatar.com/avatar/#{md5}?s=180&d=identicon"
  end

  def bootstrap_class_for flash_type
    case flash_type
    when :success
      "alert-success"
    when :error
      "alert-error"
    when :alert
      "alert-block"
    when :notice
      "alert-info"
    else
      flash_type.to_s
    end
  end

end
