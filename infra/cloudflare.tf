resource "cloudflare_dns_record" "alb_cname" {
  zone_id = var.cloudflare_zone_id
  name    = "pythia"
  type    = "CNAME"
  content = aws_lb.main.dns_name
  proxied = true
  ttl     = 1
}