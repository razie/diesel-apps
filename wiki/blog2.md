https://www.cs.cornell.edu/andru/cs711/2002fa/reading/sagas.pdf

## Distributed transactions

Long running transactions that connect a few resources (like a few databases, messaging systems and say some adapters) are a main stay of J2EE: the distributed transaction or sometimes called XA, after the most common protocol to implement them.

These are a headache, in terms of performance overhead, but performance is not why we use them… we use them because they make life easier for developers, as we assume all failures will be handled and we don't need to concern with them.

// typical XA between the DB and JMS:
xa.start
  db1.order.update
  db2.account.update
  jms1.send
  jms2.send
xa.commit


While everything works fine on paper, that's hardly the case, in real life. In massive distributed systems, failures are the norm and in my long J2EE experience, despite its promises, in real life XA doesn't deal gracefully with failures. Who hasn't heard of "in-doubt transactions" yet? They would handle indeed several failure cases in the example above, but not all, so you'll end up having to do some manual cleanup every so often.

Another big limitation of the XA model is the inability to deal with non-XA resources (sending emails through gmail). There is a possibility to insert a *single* non-xa resource in an XA transaction, in some edge cases, but that's not the rule. Implementing XA is complicated, as the requirements of the protocol are quite intricate.

The distributed transactions also introduce a seriuos coupling between distributed systems and, the longer they are and the more systems they have to coordinate, the bigger the chances that something will go wrong, so they are not really a good model for coordinating anything but simple resources and simple flows.


## Sagas

The Sagas, proposed in this paper (link ) are an evolution from XA, especially designed to deal with failures in a distributed environment, by... well, acknowledging them. Basically, when we have to execute a sequence of steps, we will:
- If a step fails, we could retry it a few times, assuming that it is idempotent (or duplicated can be rejected).
- If we cannot recover, then we'll rollback whatever we've done.

The retry will take care of cases when a system fails (reboots or re-connects) within a reasonable timeout. The rollback brings the system back into a consistent state, if one of steps fails consistently.

Simple enough, and very efficient. The drawbacks are that it is possible that the underlying data and systems could be in an inconsistent state for a while, so we have to deal wtih isolation higher up the stack (queuing, partitioning, re-ordering or simply dropping some requirements/features).
- queuing requests per logical entities, so they do not proceed in parallel on data that can be inconsistent (i.e. queue orders per account)
- partitioning of processing streams, again with the purpose that we don't proceed in parallel on entities that could be inconsistent
- re-ordering of messages is sometimes feasible (i.e. reorder based on service or product dependencies inside a single account) or reorder based on entity state (some may wait until the state has settled etc)
- dropping requirements of isolation or parallelism or such

Yeah - queuing orders in a high-volume commercial situation is a lost art.

In my experience, it is a good idea to distinguish between connection or system failures (network down, docker restarting, node failing over etc) and logical errors (bad data, inconsistent links etc). We should likely not retry the second category.

/alert blue
So, the idea behind Sagas is about coordinating sets of operations that have explicit compensations. In case the set fails definitively part-way through, the individual compensations will bring it back to a consistent state. This is an extension of the XA model, which only invokes a "rollback" command for each transaction (possibly bundling several operations into a single transaction).
/alert

A useful extension of Sagas is using parallel Sagas, where a few steps are executed on a parallel branch and this takes us to flows.


## Flows

Some are not happy to relate Sagas to Flows, but the reality is that a flow is just an extension of a Saga, **if it defines compensation handlers** and has a clear compensation strategy which will guarantee that the final state will be coherent, be it completed successfuly or compensated.
