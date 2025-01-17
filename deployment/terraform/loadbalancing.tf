# Public ALB

# Security Group
resource "aws_security_group" "public_alb" {
  vpc_id = "${aws_vpc.plutus.id}"

  ## inbound (world): ICMP 3:4 "Fragmentation Needed and Don't Fragment was Set"
  ingress {
    from_port   = "3"
    to_port     = "4"
    protocol    = "ICMP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): https
  ingress {
    from_port   = "443"
    to_port     = "443"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  ## inbound (world): http
  ingress {
    from_port   = "80"
    to_port     = "80"
    protocol    = "TCP"
    cidr_blocks = ["0.0.0.0/0"]
  }

  egress {
    from_port   = "${local.nixops_nginx_port}"
    to_port     = "${local.nixops_nginx_port}"
    protocol    = "TCP"
    cidr_blocks = ["${var.private_subnet_cidrs}"]
  }

  tags {
    Name        = "${var.project}_${var.env}_public_alb"
    Project     = "${var.project}"
    Environment = "${var.env}"
  }
}

data "aws_acm_certificate" "plutus_private" {
  domain      = "*.${var.plutus_tld}"
  statuses    = ["ISSUED"]
  most_recent = true
}

data "aws_acm_certificate" "meadow_private" {
  domain      = "*.${var.meadow_tld}"
  statuses    = ["ISSUED"]
  most_recent = true
}

data "aws_acm_certificate" "monitoring_private" {
  domain      = "*.${var.monitoring_tld}"
  statuses    = ["ISSUED"]
  most_recent = true
}

resource "aws_alb" "plutus" {
  subnets = ["${aws_subnet.public.*.id}"]
  security_groups = ["${aws_security_group.public_alb.id}"]
  internal        = false
  tags {
    Name        = "${var.project}_${var.env}_public_alb"
    Project     = "${var.project}"
    Environment = "${var.env}"
  }
}

resource "aws_lb_listener" "redirect" {
  load_balancer_arn = "${aws_alb.plutus.arn}"
  port              = "80"
  protocol          = "HTTP"

  default_action {
    type = "redirect"

    redirect {
      port        = "443"
      protocol    = "HTTPS"
      status_code = "HTTP_301"
    }
  }
}

resource "aws_alb_listener" "playground" {
  load_balancer_arn = "${aws_alb.plutus.arn}"
  port              = "443"
  protocol          = "HTTPS"
  certificate_arn   = "${data.aws_acm_certificate.plutus_private.arn}"
  default_action {
    target_group_arn = "${aws_alb_target_group.playground.arn}"
    type             = "forward"
  }
}

resource "aws_lb_listener_certificate" "meadow" {
  listener_arn    = "${aws_alb_listener.playground.arn}"
  certificate_arn = "${data.aws_acm_certificate.meadow_private.arn}"
}

resource "aws_lb_listener_certificate" "monitoring" {
  listener_arn    = "${aws_alb_listener.playground.arn}"
  certificate_arn = "${data.aws_acm_certificate.monitoring_private.arn}"
}

# Playground
resource "aws_alb_target_group" "playground" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = "${aws_vpc.plutus.id}"
  health_check = {
    path = "/api/health"
  }
  stickiness = {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "playground" {
  depends_on   = ["aws_alb_target_group.playground"]
  listener_arn = "${aws_alb_listener.playground.arn}"
  priority     = 100
  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.playground.id}"
  }
  condition {
    field  = "host-header"
    values = ["${local.plutus_domain_name}"]
  }
}

resource "aws_alb_target_group_attachment" "playground_a" {
  target_group_arn = "${aws_alb_target_group.playground.arn}"
  target_id        = "${aws_instance.playground_a.id}"
  port             = "80"
}
resource "aws_alb_target_group_attachment" "playground_b" {
  target_group_arn = "${aws_alb_target_group.playground.arn}"
  target_id        = "${aws_instance.playground_b.id}"
  port             = "80"
}

resource "aws_route53_record" "playground_alb" {
  zone_id = "${var.plutus_public_zone}"
  name    = "${local.plutus_domain_name}"
  type    = "A"
  alias {
    name                   = "${aws_alb.plutus.dns_name}"
    zone_id                = "${aws_alb.plutus.zone_id}"
    evaluate_target_health = true
  }
}

# Meadow
resource "aws_alb_target_group" "meadow" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = "${aws_vpc.plutus.id}"
  health_check = {
    path = "/api/health"
  }
  stickiness = {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "meadow" {
  depends_on   = ["aws_alb_target_group.meadow"]
  listener_arn = "${aws_alb_listener.playground.arn}"
  priority     = 101
  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.meadow.id}"
  }
  condition {
    field  = "host-header"
    values = ["${local.meadow_domain_name}"]
  }
}

resource "aws_alb_target_group_attachment" "meadow_a" {
  target_group_arn = "${aws_alb_target_group.meadow.arn}"
  target_id        = "${aws_instance.meadow_a.id}"
  port             = "80"
}
resource "aws_alb_target_group_attachment" "meadow_b" {
  target_group_arn = "${aws_alb_target_group.meadow.arn}"
  target_id        = "${aws_instance.meadow_b.id}"
  port             = "80"
}

resource "aws_route53_record" "meadow_alb" {
  zone_id = "${var.meadow_public_zone}"
  name    = "${local.meadow_domain_name}"
  type    = "A"
  alias {
    name                   = "${aws_alb.plutus.dns_name}"
    zone_id                = "${aws_alb.plutus.zone_id}"
    evaluate_target_health = true
  }
}

# Monitoring
resource "aws_alb_target_group" "monitoring" {
  # ALB is taking care of SSL termination so we listen to port 80 here
  port     = "80"
  protocol = "HTTP"
  vpc_id   = "${aws_vpc.plutus.id}"
  health_check = {
    path = "/metrics"
  }
  stickiness = {
    type = "lb_cookie"
  }
}

resource "aws_alb_listener_rule" "monitoring" {
  depends_on   = ["aws_alb_target_group.monitoring"]
  listener_arn = "${aws_alb_listener.playground.arn}"
  priority     = 102
  action {
    type             = "forward"
    target_group_arn = "${aws_alb_target_group.monitoring.id}"
  }
  condition {
    field  = "host-header"
    values = ["${local.monitoring_domain_name}"]
  }
}

resource "aws_alb_target_group_attachment" "monitoring_a" {
  target_group_arn = "${aws_alb_target_group.monitoring.arn}"
  target_id        = "${aws_instance.nixops.id}"
  port             = "${local.nixops_nginx_port}"
}

resource "aws_route53_record" "monitoring_alb" {
  zone_id = "${var.monitoring_public_zone}"
  name    = "${local.monitoring_domain_name}"
  type    = "A"
  alias {
    name                   = "${aws_alb.plutus.dns_name}"
    zone_id                = "${aws_alb.plutus.zone_id}"
    evaluate_target_health = true
  }
}