/*
 * Copyright 2011-2021 GatlingCorp (https://gatling.io)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.gatling.core.javaapi;

import static io.gatling.core.javaapi.CoreDsl.*;

import java.io.ByteArrayInputStream;
import java.time.Duration;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

public class CoreJavaCompileTest extends Simulation {

  // execs
  private ChainBuilder chain1 = exec(session -> session);
  private ChainBuilder chain2 = exec(chain1, chain1).exec(chain1);
  // pauses
  private ChainBuilder pause1 = pause(1);
  // loops
  private ChainBuilder loop1 = repeat(1).loop(chain1);
  // groups
  private ChainBuilder group1 = group("group").grouping(chain1);
  private ChainBuilder group2 = group(session -> "group").grouping(chain1);

  // bodies
  private Body.WithString stringBody1 = StringBody("static ${dynamic} static");
  private Body.WithString stringBody2 = StringBody(session -> "body");
  private Body.WithBytes rawFileBody1 = RawFileBody("path");
  private Body.WithBytes rawFileBody2 = RawFileBody(session -> "path");
  private Body.WithString elFileBody1 = ElFileBody("path");
  private Body.WithString elFileBody2 = ElFileBody(session -> "path");
  private Body.WithString pebbleStringBody = PebbleStringBody("template string");
  private Body.WithString pebbleFileBody1 = PebbleFileBody("path");
  private Body.WithString pebbleFileBody2 = PebbleFileBody(session -> "path");
  private Body.WithBytes byteArrayBody1 = ByteArrayBody(new byte[] {1});
  private Body.WithBytes byteArrayBody2 = ByteArrayBody(session -> new byte[] {1});
  private Body inputStreamBody =
      InputStreamBody(session -> new ByteArrayInputStream(new byte[] {1}));

  // scenario
  private ScenarioBuilder scenario =
      scenario("scenario")
          // execs
          .exec(session -> session)
          .exec(chain1, chain2)
          .exec(Collections.singletonList(chain1))
          // groups
          .group("group")
          .grouping(chain1)
          .group(session -> "group")
          .grouping(chain1)
          // feeds
          .feed(csv("foo"))
          .feed(csv("foo", '"'))
          .feed(ssv("foo"))
          .feed(ssv("foo", '"'))
          .feed(tsv("foo"))
          .feed(tsv("foo", '"'))
          .feed(separatedValues("foo", '|'))
          .feed(separatedValues("foo", '|', '"'))
          .feed(jsonFile("foo"))
          .feed(jsonUrl("foo"))
          .feed(
              Stream.iterate(0, i -> i + 1)
                  .limit(10)
                  .map(
                      i -> {
                        Map<String, Object> map = new HashMap<>();
                        map.put("key", i);
                        return map;
                      })
                  .iterator())
          .feed(
              () ->
                  Stream.iterate(0, i -> i + 1)
                      .limit(10)
                      .map(
                          i -> {
                            Map<String, Object> map = new HashMap<>();
                            map.put("key", i);
                            return map;
                          })
                      .iterator())
          .feed(arrayFeeder(new Map[] {new HashMap<>(), new HashMap<>()}).circular())
          .feed(listFeeder(Collections.singletonList(new HashMap<>())).circular())
          // pauses
          .pause(1)
          .pause(Duration.ofMillis(100))
          .pause("${pause}")
          .pause(session -> Duration.ofMillis(100))
          .pause(1, 2)
          .pause(Duration.ofMillis(100), Duration.ofMillis(200))
          .pause("${min}", "${max}")
          .pause(session -> Duration.ofMillis(100), session -> Duration.ofMillis(200))
          .pause(1, constantPauses)
          .pause(Duration.ofMillis(100), constantPauses)
          .pause("${pause}", constantPauses)
          .pause(session -> Duration.ofMillis(100), constantPauses)
          .pause(1, 2, constantPauses)
          .pause(Duration.ofMillis(100), Duration.ofMillis(200), constantPauses)
          .pause("${min}", "${max}", constantPauses)
          .pause(
              session -> Duration.ofMillis(100), session -> Duration.ofMillis(200), constantPauses)
          // pace
          .pace(1)
          .pace(1, "counter")
          .pace(Duration.ofSeconds(1))
          .pace(Duration.ofSeconds(1), "counter")
          .pace("${pace}")
          .pace("${pace}", "counter")
          .pace(session -> Duration.ofSeconds(1))
          .pace(session -> Duration.ofSeconds(1), "counter")
          .pace(1, 2)
          .pace(1, 2, "counter")
          .pace(Duration.ofSeconds(1), Duration.ofSeconds(2))
          .pace(Duration.ofSeconds(1), Duration.ofSeconds(2), "counter")
          .pace("${min}", "${max}", "counter")
          .pace(session -> Duration.ofSeconds(1), session -> Duration.ofSeconds(2))
          .pace(session -> Duration.ofSeconds(1), session -> Duration.ofSeconds(2), "counter")
          // rendezVous
          .rendezVous(5)
          // repeat
          .repeat(1)
          .loop(chain1)
          .repeat(1, "counterName")
          .loop(chain1)
          .repeat(session -> 1)
          .loop(chain1)
          .repeat(session -> 1, "counterName")
          .loop(chain1)
          // foreach
          .foreach(Collections.singletonList(1), "attributeName")
          .loop(chain1)
          .foreach(Collections.singletonList(1), "attributeName", "counterName")
          .loop(chain1)
          .foreach(session -> Collections.singletonList(1), "attributeName")
          .loop(chain1)
          .foreach(session -> Collections.singletonList(1), "attributeName", "counterName")
          .loop(chain1)
          // forever
          .forever()
          .loop(chain1)
          .forever("counterName")
          .loop(chain1)
          // asLongAs
          .asLongAs("${condition}")
          .loop(chain1)
          .asLongAs("${condition}", "counterName")
          .loop(chain1)
          .asLongAs("${condition}", true)
          .loop(chain1)
          .asLongAs("${condition}", "counterName", true)
          .loop(chain1)
          .asLongAs(session -> true)
          .loop(chain1)
          .asLongAs(session -> true, "counterName")
          .loop(chain1)
          .asLongAs(session -> true, true)
          .loop(chain1)
          .asLongAs(session -> true, "counterName", true)
          .loop(chain1)
          // doWhile
          .doWhile("${condition}")
          .loop(chain1)
          .doWhile("${condition}", "counterName")
          .loop(chain1)
          .doWhile(session -> true)
          .loop(chain1)
          .doWhile(session -> true, "counterName")
          .loop(chain1)
          // asLongAsDuring
          .asLongAsDuring("${condition}", "${duration}")
          .loop(chain1)
          .asLongAsDuring("${condition}", "${duration}", "counterName")
          .loop(chain1)
          .asLongAsDuring("${condition}", "${duration}", true)
          .loop(chain1)
          .asLongAsDuring("${condition}", "${duration}", "counterName", true)
          .loop(chain1)
          .asLongAsDuring(session -> true, session -> Duration.ofSeconds(1))
          .loop(chain1)
          .asLongAsDuring(session -> true, session -> Duration.ofSeconds(1), "counterName")
          .loop(chain1)
          .asLongAsDuring(session -> true, session -> Duration.ofSeconds(1), true)
          .loop(chain1)
          .asLongAsDuring(session -> true, session -> Duration.ofSeconds(1), "counterName", true)
          .loop(chain1)
          .doWhileDuring("${condition}", "${duration}")
          .loop(chain1)
          .doWhileDuring("${condition}", "${duration}", "counterName")
          .loop(chain1)
          .doWhileDuring("${condition}", "${duration}", true)
          .loop(chain1)
          .doWhileDuring("${condition}", "${duration}", "counterName", true)
          .loop(chain1)
          .doWhileDuring(session -> true, session -> Duration.ofSeconds(1))
          .loop(chain1)
          .doWhileDuring(session -> true, session -> Duration.ofSeconds(1), "counterName")
          .loop(chain1)
          .doWhileDuring(session -> true, session -> Duration.ofSeconds(1), true)
          .loop(chain1)
          .doWhileDuring(session -> true, session -> Duration.ofSeconds(1), "counterName", true)
          .loop(chain1)
          // doIf
          .doIf("${condition}")
          .then(chain1)
          .doIf(session -> true)
          .then(chain1)
          // doIfOrElse
          .doIfOrElse("${condition}")
          .then(chain1)
          .orElse(chain2)
          .doIfOrElse(session -> true)
          .then(chain1)
          .orElse(chain2)
          // doIfEquals
          .doIfEquals("${actual}", 1)
          .then(chain1)
          .doIfEquals("${actual}", "${expected}")
          .then(chain1)
          .doIfEquals("${actual}", session -> 1)
          .then(chain1)
          .doIfEquals(session -> "actual", 1)
          .then(chain1)
          .doIfEquals(session -> "actual", "${expected}")
          .then(chain1)
          .doIfEquals(session -> "actual", session -> 1)
          .then(chain1)
          // doIfEqualsOrElse
          .doIfEqualsOrElse("${actual}", 1)
          .then(chain1)
          .orElse(chain2)
          .doIfEqualsOrElse("${actual}", "${expected}")
          .then(chain1)
          .orElse(chain2)
          .doIfEqualsOrElse("${actual}", session -> 1)
          .then(chain1)
          .orElse(chain2)
          .doIfEqualsOrElse(session -> "actual", 1)
          .then(chain1)
          .orElse(chain2)
          .doIfEqualsOrElse(session -> "actual", "${expected}")
          .then(chain1)
          .orElse(chain2)
          .doIfEqualsOrElse(session -> "actual", session -> 1)
          .then(chain1)
          .orElse(chain2)
          // doSwitch
          .doSwitch("${value}")
          .choices(Choice.withValue("value1", chain1), Choice.withValue("value2", chain2))
          .doSwitch("${value}")
          .choices(Collections.singletonList(Choice.withValue("value1", chain1)))
          .doSwitch(session -> "value")
          .choices(Choice.withValue("value1", chain1), Choice.withValue("value2", chain2))
          .doSwitch(session -> "value")
          .choices(Collections.singletonList(Choice.withValue("value1", chain1)))
          // doSwitchOrElse
          .doSwitchOrElse("${value}")
          .choices(Choice.withValue("value1", chain1), Choice.withValue("value2", chain2))
          .orElse(chain2)
          .doSwitchOrElse("${value}")
          .choices(Collections.singletonList(Choice.withValue("value1", chain1)))
          .orElse(chain2)
          .doSwitchOrElse(session -> "value")
          .choices(Choice.withValue("value1", chain1), Choice.withValue("value2", chain2))
          .orElse(chain2)
          .doSwitchOrElse(session -> "value")
          .choices(Collections.singletonList(Choice.withValue("value1", chain1)))
          .orElse(chain2)
          // randomSwitch
          .randomSwitch()
          .choices(Choice.withWeight(50.0, chain1), Choice.withWeight(50.0, chain2))
          .randomSwitch()
          .choices(Collections.singletonList(Choice.withWeight(50.0, chain1)))
          // randomSwitchOrElse
          .randomSwitchOrElse()
          .choices(Choice.withWeight(50.0, chain1), Choice.withWeight(50.0, chain2))
          .orElse(chain2)
          .randomSwitchOrElse()
          .choices(Collections.singletonList(Choice.withWeight(50.0, chain1)))
          .orElse(chain2)
          // uniformRandomSwitch
          .uniformRandomSwitch()
          .choices(chain1, chain2)
          .uniformRandomSwitch()
          .choices(Collections.singletonList(chain1))
          // roundRobinSwitch
          .roundRobinSwitch()
          .choices(chain1, chain2)
          .roundRobinSwitch()
          .choices(Collections.singletonList(chain1))
          // exitBlockOnFail
          .exitBlockOnFail(chain1)
          // tryMax
          .tryMax(1)
          .trying(chain1)
          .tryMax(1, "counterName")
          .trying(chain1)
          .tryMax("${times}")
          .trying(chain1)
          .tryMax("${times}", "counterName")
          .trying(chain1)
          .tryMax(session -> 1)
          .trying(chain1)
          .tryMax(session -> 1, "counterName")
          .trying(chain1)
          // exitHereIf
          .exitHereIf("${condition}")
          .exitHereIf(session -> true)
          // exitHere
          .exitHere()
          // exitHereIfFailed
          .exitHereIfFailed();

  CoreJavaCompileTest() {
    registerPebbleExtensions((com.mitchellbosecke.pebble.extension.Extension) null);
    setUp(
            scenario.injectOpen(
                rampUsers(5).during(1),
                rampUsers(5).during(Duration.ofSeconds(1)),
                stressPeakUsers(5).during(1),
                stressPeakUsers(5).during(Duration.ofSeconds(1)),
                atOnceUsers(1000),
                constantUsersPerSec(10).during(1),
                constantUsersPerSec(10).during(Duration.ofSeconds(1)),
                rampUsersPerSec(100).to(200).during(1),
                rampUsersPerSec(100).to(200).during(Duration.ofSeconds(1)),
                nothingFor(1),
                nothingFor(Duration.ofSeconds(1)),
                incrementUsersPerSec(1.0).times(5).eachLevelLasting(1),
                incrementUsersPerSec(1.0).times(5).eachLevelLasting(1).startingFrom(1.0),
                incrementUsersPerSec(1.0).times(5).eachLevelLasting(1).separatedByRampsLasting(1),
                incrementUsersPerSec(1.0)
                    .times(5)
                    .eachLevelLasting(1)
                    .startingFrom(1.0)
                    .separatedByRampsLasting(1),
                incrementUsersPerSec(1.0)
                    .times(5)
                    .eachLevelLasting(Duration.ofSeconds(1))
                    .startingFrom(1.0)
                    .separatedByRampsLasting(Duration.ofSeconds(1))),
            scenario
                .injectClosed(
                    constantConcurrentUsers(100).during(1),
                    constantConcurrentUsers(100).during(Duration.ofSeconds(1)),
                    rampConcurrentUsers(1).to(5).during(1),
                    rampConcurrentUsers(1).to(5).during(Duration.ofSeconds(1)),
                    incrementConcurrentUsers(1).times(5).eachLevelLasting(1),
                    incrementConcurrentUsers(1).times(5).eachLevelLasting(1),
                    incrementConcurrentUsers(1).times(5).eachLevelLasting(1).startingFrom(1),
                    incrementConcurrentUsers(1)
                        .times(5)
                        .eachLevelLasting(1)
                        .separatedByRampsLasting(1),
                    incrementConcurrentUsers(1)
                        .times(5)
                        .eachLevelLasting(1)
                        .startingFrom(1)
                        .separatedByRampsLasting(1),
                    incrementConcurrentUsers(1)
                        .times(5)
                        .eachLevelLasting(Duration.ofSeconds(1))
                        .startingFrom(1)
                        .separatedByRampsLasting(Duration.ofSeconds(1)))
                .andThen(scenario.injectOpen(atOnceUsers(1))))
        .assertions(
            global().allRequests().count().is(5L),
            global().allRequests().percent().is(5.5),
            forAll().allRequests().count().is(5L),
            details("group", "request").allRequests().count().is(5L))
        .maxDuration(1)
        .maxDuration(Duration.ofSeconds(1))
        .throttle(
            reachRps(100).in(1),
            reachRps(100).in(Duration.ofSeconds(1)),
            jumpToRps(100),
            holdFor(1),
            holdFor(Duration.ofSeconds(1)))
        .disablePauses()
        .constantPauses()
        .exponentialPauses()
        .customPauses(session -> 1L)
        .uniformPauses(1)
        .uniformPauses(Duration.ofSeconds(1));
  }
}
