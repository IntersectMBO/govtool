# Build Stage

FROM maven:3.9.4-eclipse-temurin-17-alpine AS build
WORKDIR /usr/src/app

COPY pom.xml .
RUN mvn dependency:go-offline

COPY src ./src
RUN mvn gatling:package

# Final Stage
FROM maven:3.9.4-eclipse-temurin-17-alpine

WORKDIR /usr/src/app
COPY --from=build /root/.m2 /root/.m2
COPY --from=build /usr/src/app/target /usr/src/app/target
COPY pom.xml .

CMD ["mvn", "gatling:test"]
