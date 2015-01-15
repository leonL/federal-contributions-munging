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

  def latitude
    response["centroid"]["coordinates"][1]
  end

  def longitude
    response["centroid"]["coordinates"][0]
  end

  def ridings
    @ridings ||= begin
      response_ridings = response["boundaries_centroid"] | response["boundaries_concordance"]
      ridings = []; response_ridings.each do |riding|
        ridings << {name: riding["name"], id: riding["external_id"]}
      end
      ridings
    end
  end

  def find_by_postal_code(postal_code)
    HTTParty.get("http://represent.opennorth.ca/postcodes/#{postal_code}",
        query: {sets: "federal-electoral-districts"})
  end

  def not_found?
    response.response.code == "404"
  end
end

def complete_record(incomplete_record, riding, concord)
  lat = incomplete_record[:latitude] || concord.latitude
  long = incomplete_record[:longitude] || concord.longitude
  city = incomplete_record[:city] || concord.city
  province = incomplete_record[:province] || concord.province

  [incomplete_record[:postal_code], riding[:id], riding[:name], lat, long, city, province]
end

CSV::Converters[:na_to_nil] = Proc.new {|val| val == "NA" ? nil : val}
complete_cases = CSV.open(
  '1_merge_output/test_complete_cases.csv', 'a',
  {quote_char: '"', force_quotes: true}
)

CSV.foreach(
  '1_merge_output/test_incomplete_cases.csv',
  headers: true, header_converters: :symbol,
  converters: :na_to_nil
) do |record|
  concordance = RepresetPostalCodeConcordance.new(record[:postal_code])
  unless concordance.not_found?
    concordance.ridings.each do |riding|
      complete_cases << complete_record(record, riding, concordance)
    end
  end
end
complete_cases.close