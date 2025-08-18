resource "cloudflare_dns_record" "alb_cname" {
  zone_id = var.cloudflare_zone_id
  name    = "pythia"
  type    = "CNAME"
  content = aws_lb.main.dns_name
  proxied = true
  ttl     = 1
}

resource "cloudflare_dns_record" "cert_validation" {
  for_each = {
    for dvo in aws_acm_certificate.app_cert.domain_validation_options : dvo.domain_name => dvo
  }

  zone_id = var.cloudflare_zone_id

  name    = each.value.resource_record_name
  type    = each.value.resource_record_type
  content = each.value.resource_record_value

  proxied = false
  ttl     = 60
}