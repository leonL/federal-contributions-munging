require 'csv'
require 'httparty'

class RepresetPostalCodeConcordance

  attr_accessor :response

  def initialize(postal_code)
    self.response = find_by_postal_code(postal_code)
  end

  def province
    response["province"]
  end

  def city
    response["city"]
  end

  def ridings
    @ridings ||= begin
      response_ridings = response["boundaries_centroid"] | response["boundaries_concordance"]
      ridings = []; response_ridings.each do |riding|
        ridings << {riding_name: riding["name"], riding_id: riding["external_id"]}
      end
      ridings
    end
  end

  def find_by_postal_code(postal_code)
    HTTParty.get("http://represent.opennorth.ca/postcodes/#{postal_code}",
        query: {sets: "federal-electoral-districts"})
  end
end

all_pcodes = CSV.read('test_postal_codes.csv')
all_pcodes.flatten!

CSV.open('test_postal_code_geo_riding_concordance.csv', 'w') do |csv|
  all_pcodes.each do |code|
    concordance = RepresetPostalCodeConcordance.new(code)
    concordance.ridings.each do |riding|
      csv << [code, concordance.city, concordance.province, riding[:riding_name], riding[:riding_id]]
    end
  end
end