const faker = require('faker');

faker.locale = 'ru';

function createTodo () {
    return {
        id: faker.random.uuid(),
        title: `${faker.hacker.verb()} ${faker.hacker.adjective()} ${faker.hacker.noun()}`,
        status: false
    };
}

module.exports = () => {
    const data = { todo: [] };
    // Create 1000 users
    for (let i = 0; i < 20; i++) {
        data.todo.push(createTodo());
    }
    return data;
};
