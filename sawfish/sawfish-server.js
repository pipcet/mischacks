var http = require("http");
var express = require("express");
var getenv = require("getenv");
var app = express();
var child_process = require("child_process");

function authenticate(req, res, next)
{
    var ip = req.headers['x-forwarded-for'] || req.connection.remoteAddress;
    if (ip.match(/127.0.0.1$/) || ip.match(/::1$/)) {
        next();
        return;
    }

    return res.sendStatus(401, "Unauthorized");
}

async function sawfish_command(req, res, next)
{
    child_process.execFile("sawfish-client", ["-e", req.rawBody ],
			   (error, stdout, stderr) => {
			       res.write(stdout);
			       res.end();
			       next();
			   });
}

app.use(authenticate);
app.use(function(req, res, next) {
    req.rawBody = "";
    req.setEncoding("utf8");

    req.on("data", function(chunk) {
        req.rawBody += chunk;
    });

    req.on("end", function() {
        next();
    });
});


app.post("/command", (req, res, next) => {
    sawfish_command(req, res, next);
});

app.listen(8252);
